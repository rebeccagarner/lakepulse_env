# Fill in missing Lake Pulse environmental data

# Load libraries
library(tidyverse)
library(janitor)


#### Import curated environmental data ####
# Import most current environmental dataset
metadata <- read_csv("output/lakepulse_data_curated_2021-01-18.csv", col_names = TRUE)


#### Count NAs ####
# Count lakes by ecozone
ecozones_nlakes <- metadata %>%
  group_by(ecozone) %>%
  count(name = "n_lakes")

# Count NAs in entire dataset of 664 lakes
nas_all <- metadata %>%
  select(-sampling_date, -lake_name, -latitude, -longitude, -province, -area,
         -size_class, -hi_index, -hi_class, -altitude, -utc, -shoreline_length,
         -ecoprovince,
         -contains("area"), -contains("fraction")) %>%
  select(lakepulse_id, ecozone, where(is.numeric)) %>%
  pivot_longer(!c(lakepulse_id, ecozone), names_to = "variable", values_to = "value") %>%
  mutate(value = case_when(!is.na(value) ~ 1)) %>%
  group_by(variable) %>%
  tally(value, name = "n_values") %>%
  mutate(n_nas = 664 - n_values)

# Count NAs by ecozone
nas_ecozones <- metadata %>%
  select(-sampling_date, -lake_name, -latitude, -longitude, -province, -area,
         -size_class, -hi_index, -hi_class, -altitude, -utc, -shoreline_length,
         -ecoprovince,
         -contains("area"), -contains("fraction")) %>%
  select(lakepulse_id, ecozone, where(is.numeric)) %>%
  pivot_longer(!c(lakepulse_id, ecozone), names_to = "variable", values_to = "value") %>%
  mutate(value = case_when(!is.na(value) ~ 1)) %>%
  group_by(variable, ecozone) %>%
  tally(value, name = "n_values") %>%
  left_join(ecozones_nlakes, by = "ecozone") %>%
  mutate(n_nas = n_lakes - n_values) %>%
  select(-n_lakes)

# Write NA information to files
# nas_all %>%
#   write_csv("~/Desktop/na_counts_664lakes.csv", col_names = TRUE)

# nas_ecozones %>%
#   write_csv("~/Desktop/na_counts_ecozones.csv", col_names = TRUE)
