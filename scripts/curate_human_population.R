# Curate QC'ed Lake Pulse watershed human population data

# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)


#### Import QC'ed data ####
# Import human population data
human_population <- read_delim("data/working/LakePulse_population_20210113.csv", delim = ";", col_names = TRUE,
                 locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

# Import and format lake sampling year information
sampling_year <- read_delim("data/working/basic_info/LakePulse_basic_info_qc_20201015.csv", delim = ";", col_names = TRUE,
                            locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8")) %>%
  mutate(sampling_year = year(sampling_date)) %>%
  select(lakepulse_id, sampling_year)


#### Filter and format data ####
# Retain human population data for year of sampling
human_population_curated <- sampling_year %>%
  left_join(human_population, by = "lakepulse_id") %>%
  pivot_longer(!c(lakepulse_id, sampling_year), names_to = "population_year", values_to = "population") %>%
  filter(sampling_year == population_year) %>%
  select(lakepulse_id, population) %>%
  arrange(lakepulse_id)

# Write curated data to file
# human_population_curated %>%
#   write_csv("data/curated/human_population_curated.csv", col_names = TRUE)
