# Derive water column variables with RBR data

# Load libraries
library(tidyverse)
library(janitor)
library(scales)


#### Import RBR files ####
# Duplicate "pre-processed" RBR casts were removed or retained based on Maxime's file.
file_path <- "data/curated/Full_RBR_Profiles/"

(rbr_files <- dir(path = file_path, pattern = "*.txt", recursive = TRUE))

# Create a rbr file contents
rbr <- tibble(file_name = rbr_files) %>%
  mutate(lake_id = str_extract(file_name, "\\d\\d\\-\\d\\d\\d")) %>%
  mutate(file_contents = map(file_name, ~ read_tsv(file.path(file_path, .),
                                                   col_names = c("depth Conductivity", "Conductivity",
                                                                 "depth Temperature", "Temperature",
                                                                 "depth Pressure", "Pressure",
                                                                 "depth Dissolved O2", "Dissolved O2",
                                                                 "depth Dissolved O2 concentration", "Dissolved O2 concentration",
                                                                 "depth Chlorophyll a", "Chlorophyll a",
                                                                 "depth ph", "ph",
                                                                 "depth Sea Pressure", "Sea Pressure",
                                                                 "depth Salinity", "Salinity",
                                                                 "depth Speed of sound", "Speed of sound",
                                                                 "depth Specific conductivity", "Specific conductivity"),
                                                   skip = 2))) %>%
  unnest(c(file_contents)) %>%
  clean_names()

# Import epilimnion/hypolimnion/thermocline depth summaries
mass_depths <- read_tsv("data/curated/Thermocline_hypolimnion_depth/preprocessedStratificationLayers.txt", col_names = TRUE) %>%
  clean_names()

# Retain a single cast for the following lakes:
# 08-186: retained data_profile.txt (longer profile) and removed data2_profile.txt
# 08-194: retained data_profile.txt (longer profile) and removed data2_profile.txt
# 17-079: retained data2_profile.txt (longer profile) and removed data_profile.txt
# 17-090: retained data_profile.txt (longer profile) and removed data2_profile.txt
# 17-092: retained data2_profile.txt (longer profile) and removed data_profile.txt
# 17-116: retained data2_profile.txt (longer profile) and removed data_profile.txt
# 17-119: retained data_profile.txt (longer profile) and removed data2_profile.txt
# 06-127: retain data_profile remove data2_profile because layer depths are NaN
lakes_cast1 <- c("08-186", "08-194", "17-090", "17-119", "06-127")
lakes_cast2 <- c("17-067", "17-079", "17-092", "17-116")

mass_depths <- mass_depths %>%
  mutate(lake_id = str_extract(file_name, "\\d\\d\\-\\d\\d\\d"),
         cast_n = case_when(grepl("data_profile", file_name) | grepl("cast_1_profile", file_name) | grepl("cast_profile", file_name) ~ "cast1",
                            grepl("data2_profile", file_name) | grepl("cast_2_profile", file_name) ~ "cast2")) %>%
  mutate(cast_eval = case_when(cast_n == "cast2" & lake_id %in% lakes_cast1 ~ "remove",
                               cast_n == "cast1" & lake_id %in% lakes_cast2 ~ "remove",
                               TRUE ~ "retain")) %>%
  filter(cast_eval == "retain")

# Calculate mean mass depths for lakes with duplicate RBR casts
mass_depths <- mass_depths %>%
  group_by(lake_id) %>%
  summarize(epilimnion_m = mean(epilimnion_m),
            thermocline_m = mean(thermocline_m),
            hypolimnion_m = mean(hypolimnion_m))

# Assess lake stratification based on NaN water mass depths
mass_depths <- mass_depths %>%
  mutate(stratification = case_when(!is.nan(epilimnion_m) ~ "stratified",
                                    is.nan(epilimnion_m) ~ "mixed"))


#### Calculate temperature means ####
# Bin RBR profiles into 10 cm bins
temp_rbr_bin10cm <- rbr %>%
  select(lake_id, depth_temperature, temperature) %>%
  filter(!is.na(depth_temperature) & !is.na(temperature)) %>%
  mutate(depth_bin10cm = round(depth_temperature, 1)) %>%
  group_by(lake_id, depth_bin10cm) %>%
  summarize(temperature_bin10cm = mean(temperature)) %>%
  rename(depth_temperature = depth_bin10cm, temperature = temperature_bin10cm)

# Assign epilimnion/metalimnion/hypolimnion to stratified lake depths based on mass depths summary
temp_rbr_mass_depths <- temp_rbr_bin10cm %>%
  left_join(mass_depths, by = "lake_id")

temp_rbr_mass_depths <- temp_rbr_mass_depths %>%
  mutate(layer = case_when(stratification == "mixed" ~ "mixed",
                           stratification == "stratified" & depth_temperature <= epilimnion_m ~ "epilimnion",
                           stratification == "stratified" & depth_temperature > epilimnion_m & depth_temperature < hypolimnion_m ~ "metalimnion",
                           stratification == "stratified" & depth_temperature >= hypolimnion_m ~ "hypolimnion"))

lakes_stratification <- temp_rbr_mass_depths %>%
  ungroup() %>%
  distinct(lake_id, stratification)

# Calculate mean temperature in each water mass (epi and hypo) for stratified lakes
temp_rbr_layer_means <- temp_rbr_mass_depths %>%
  filter(stratification == "stratified") %>%
  filter(layer == "epilimnion" | layer == "hypolimnion") %>%
  group_by(lake_id, layer) %>%
  summarize(temperature_mean = round(mean(temperature), 1)) %>%
  spread(layer, temperature_mean, fill = NA) %>%
  ungroup()

# For both stratified and mixed lakes, calculate mean temperature over entire water column
temp_watercolumn_means <- temp_rbr_bin10cm %>%
  group_by(lake_id) %>%
  summarize(watercolumn_temp = mean(temperature))


#### Calculate surface temperature ####
# We have RBR data for 616 lakes (out of 664)
temp_rbr_mass_depths %>%
  group_by(lake_id) %>%
  summarize(shallowest_depth = min(depth_temperature)) %>%
  filter(shallowest_depth <= 0.5)
# 538 lakes (out of 616) have temperature data for depths above -0.5 m

temp_rbr_mass_depths %>%
  group_by(lake_id) %>%
  summarize(shallowest_depth = min(depth_temperature)) %>%
  filter(shallowest_depth <= 1)
# 587 lakes (out of 616) have temperature data for depths above -1 m

# Derive surface temperature means for top 0.5 and 1 m
temp_top0.5m <- temp_rbr_mass_depths %>%
  filter(depth_temperature <= 0.5) %>%
  group_by(lake_id) %>%
  summarize(temperature_top0.5m = mean(temperature))

temp_top1m <- temp_rbr_mass_depths %>%
  filter(depth_temperature <= 1) %>%
  group_by(lake_id) %>%
  summarize(temperature_top1m = mean(temperature))

# Calculate topmost temperature
# Identify topmost temperature
rbr_depth_temp_min <- temp_rbr_mass_depths %>%
  group_by(lake_id) %>%
  summarize(depth_temp_min = min(depth_temperature))

temp_topmost <- rbr_depth_temp_min %>%
  left_join(temp_rbr_mass_depths %>%
              select(lake_id, depth_temperature, temperature),
            by = c("lake_id", "depth_temp_min" = "depth_temperature")) %>%
  select(-depth_temp_min) %>%
  rename(temp_topmost = temperature)

temp_surface <- temp_top0.5m %>%
  full_join(temp_top1m, by = "lake_id") %>%
  full_join(temp_topmost, by = "lake_id")


#### Write temperature means to file ####
temp_means <- lakes_stratification %>%  # Lake stratification status
  full_join(temp_watercolumn_means) %>%  # Mean water column temperatures
  full_join(temp_surface, by = "lake_id") %>%  # Mean surface temperatures
  full_join(temp_rbr_layer_means, by = "lake_id") %>%  # Mean epi/hypo temperatures
  mutate(watercolumn_temp = round(watercolumn_temp, 1),
         temperature_top0.5m = round(temperature_top0.5m, 1),
         temperature_top1m = round(temperature_top1m, 1),
         temp_topmost = round(temp_topmost, 1))

# temp_means %>%
#   rename(rbr_temperature_mean_watercolumn = watercolumn_temp,
#          rbr_temperature_mean_top0.5m = temperature_top0.5m,
#          rbr_temperature_mean_top1m = temperature_top1m,
#          rbr_temperature_mean_topmost = temp_topmost,
#          rbr_temperature_mean_epilimnion = epilimnion,
#          rbr_temperature_mean_hypolimnion = hypolimnion) %>%
#   write_csv("data/curated/rbr_temperature_means.csv", col_names = TRUE)
