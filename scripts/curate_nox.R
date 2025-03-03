# Curate QC'ed Lake Pulse NOx data

# Load libraries
library(tidyverse)
library(janitor)


#### Import QC'ed data ####
nox <- read_delim("data/working/LakePulse_nox_qc_20210517.csv", delim = ";", col_names = TRUE,
                 locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

lakes <- nox %>%
  distinct(lakepulse_id)


#### Format data ####
# Trim white space in flags
nox <- nox %>%
  mutate(nox_flags = trimws(nox_flags),
         no2_flags = trimws(no2_flags),
         no3_flags = trimws(no3_flags))

# Separate information for NOx, NO2, NO3
nox_only <- nox %>%
  select(lakepulse_id, depth, contains("nox"))

no2_only <- nox %>%
  select(lakepulse_id, depth, contains("no2"))

no3_only <- nox %>%
  select(lakepulse_id, depth, contains("no3"))


#### Filter data ####
# Remove missing data
nox_curated <- nox_only %>%
  filter(!is.na(nox_concentration))

no2_curated <- no2_only %>%
  filter(!is.na(no2_concentration))

no3_curated <- no3_only %>%
  filter(!is.na(no3_concentration))


#### Format data again ####
nox_formatted <- nox_curated %>%
  left_join(no2_curated, by = c("lakepulse_id", "depth")) %>%
  left_join(no3_curated, by = c("lakepulse_id", "depth")) %>%
  select(lakepulse_id, depth, nox_concentration, no2_concentration, no3_concentration) %>%
  pivot_longer(!c(lakepulse_id, depth), names_to = "var", values_to = "value") %>%
  mutate(depth = tolower(depth),
         var = str_remove(var, "_concentration")) %>%
  unite("var_depth", c(var, depth), sep = "_") %>%
  pivot_wider(names_from = var_depth, values_from = value, values_fill = NA)

nox_formatted <- lakes %>%
  left_join(nox_formatted, by = "lakepulse_id") %>%
  arrange(lakepulse_id)

# Write curated data to file
# nox_formatted %>%
#   write_csv("data/curated/nox_curated.csv", col_names = TRUE)
