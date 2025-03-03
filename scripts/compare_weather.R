# Compare original and 2022-04 corrected ERA-5 Land precipitation and solar radiation data

# Load libraries
library(tidyverse)
library(janitor)


#### Import and format QC'ed data ####
# Import total precipitation data
precipitation_7d <- read_tsv("data/working/era5land/total_precipitation_processing7daysSums.txt") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse,
         precipitation_total_7d = total_precipitation_m)

precipitation_30d <- read_tsv("data/working/era5land/total_precipitation_processing30daysSums.txt") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse,
         precipitation_total_30d = total_precipitation_m)

precipitation_new <- read_delim("data/working/era5land/LakePulse_climate_data_total_precipitation_20220406.csv", delim = ";", col_names = TRUE,
                                locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8")) %>%
  dplyr::select(lakepulse_id, starts_with("sum"))

# Import net solar radiation data
radiation_7d <- read_tsv("data/working/era5land/surface_net_solar_radiation_processing7daysMeans.txt") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse,
         solar_radiation_net_7d = surface_net_solar_radiation_j_m_2)

radiation_30d <- read_tsv("data/working/era5land/surface_net_solar_radiation_processing30daysMeans.txt") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse,
         solar_radiation_net_30d = surface_net_solar_radiation_j_m_2)

radiation_new <- read_delim("data/working/era5land/LakePulse_climate_data_solar_radiation_20220406.csv", delim = ";", col_names = TRUE,
                            locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8")) %>%
  dplyr::select(lakepulse_id, starts_with("mean"))


#### Compare data ####
# Combine precipitation data
precipitation_all <- precipitation_7d %>%
  left_join(precipitation_30d, by = "lakepulse_id") %>%
  left_join(precipitation_new, by = "lakepulse_id") %>%
  arrange(lakepulse_id)

# Combine radiation data
radiation_all <- radiation_7d %>%
  left_join(radiation_30d, by = "lakepulse_id") %>%
  left_join(radiation_new, by = "lakepulse_id") %>%
  arrange(lakepulse_id)

# Convert precipitation data to long-format
precipitation_long <- precipitation_all %>%
  rename(original_7d = precipitation_total_7d,
         original_30d = precipitation_total_30d,
         corrected_7d = sum7daysPrior,
         corrected_30d = sum30daysPrior) %>%
  pivot_longer(!lakepulse_id, names_to = c(".value", "var"), names_pattern = "(.*)_(.*)")

# Convert radiation data to long-format
radiation_long <- radiation_all %>%
  rename(original_7d = solar_radiation_net_7d,
         original_30d = solar_radiation_net_30d,
         corrected_7d = mean7daysPrior,
         corrected_30d = mean30daysPrior) %>%
  pivot_longer(!lakepulse_id, names_to = c(".value", "var"), names_pattern = "(.*)_(.*)")

# Plot scatter plots comparing original and 2022-04 corrected data
precipitation_long %>%
  ggplot() +
  facet_wrap(~var, scales = "free") +
  geom_point(aes(x = corrected, y = original)) +
  theme_bw()

radiation_long %>%
  ggplot() +
  facet_wrap(~var, scales = "free") +
  geom_point(aes(x = corrected, y = original)) +
  theme_bw()
