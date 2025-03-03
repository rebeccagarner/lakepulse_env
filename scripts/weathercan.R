# Extract weather data from Environment Canada
# https://ropensci.org/blog/2018/03/06/weathercan/
# Hourly data is not relevant (different variables)

# Load libraries
library(tidyverse)
#library(lubridate)
#library(weathercan)


#### Import and format lake sampling information ####
basicinfo_landuse <- read_csv("data/curated/basicinfo_landuse_curated.csv", col_names = TRUE) %>%
  mutate(sampling_year = year(sampling_date))

lakes_geo_date <- basicinfo_landuse %>%
  select(lakepulse_id, latitude, longitude, sampling_year, sampling_date)


#### Import 7-day weather data ####
lakes_weather_7days <- read_tsv("data/working/weather/lakes_weather_7days.tsv", col_names = TRUE)

lakes_weather_temp_7days <- lakes_weather_7days %>%
  distinct(lakepulse_id, date, .keep_all = TRUE) %>%
  group_by(lakepulse_id) %>%
  summarize(mean_cool_deg_days_7d = mean(cool_deg_days, na.rm = TRUE),
            sum_cool_deg_days_7d = sum(cool_deg_days, na.rm = TRUE),
            mean_heat_deg_days_7d = mean(heat_deg_days, na.rm = TRUE),
            sum_heat_deg_days_7d = sum(heat_deg_days, na.rm = TRUE),
            mean_max_temp_7d = mean(max_temp, na.rm = TRUE),
            mean_mean_temp_7d = mean(mean_temp, na.rm = TRUE),
            mean_min_temp_7d = mean(min_temp, na.rm = TRUE))

lakes_totalprecip_ndays_7days <- lakes_weather_7days %>%
  distinct(lakepulse_id, date, .keep_all = TRUE) %>%
  select(lakepulse_id, total_precip) %>%
  filter(!is.na(total_precip)) %>%
  group_by(lakepulse_id) %>%
  summarize(n_days = n())

# Add correction for total precipitation number of days
lakes_totalprecip_7days <- lakes_weather_7days %>%
  distinct(lakepulse_id, date, .keep_all = TRUE) %>%
  select(lakepulse_id, total_precip) %>%
  filter(!is.na(total_precip)) %>%
  group_by(lakepulse_id) %>%
  summarize(sum_total_precip_7d = sum(total_precip, na.rm = TRUE)) %>%
  left_join(lakes_totalprecip_ndays_7days, by = "lakepulse_id") %>%
  mutate(sum_total_precip_7d = sum_total_precip_7d/(n_days/7)) %>%
  select(-n_days)

lakes_weather_summary_7days <- lakes_weather_temp_7days %>%
  full_join(lakes_totalprecip_7days, by = "lakepulse_id")


#### Import 30-day weather data ####
lakes_weather_30days <- read_tsv("data/working/weather/lakes_weather_30days.tsv", col_names = TRUE)

lakes_weather_temp_30days <- lakes_weather_30days %>%
  distinct(lakepulse_id, date, .keep_all = TRUE) %>%
  group_by(lakepulse_id) %>%
  summarize(mean_cool_deg_days_30d = mean(cool_deg_days, na.rm = TRUE),
            sum_cool_deg_days_30d = sum(cool_deg_days, na.rm = TRUE),
            mean_heat_deg_days_30d = mean(heat_deg_days, na.rm = TRUE),
            sum_heat_deg_days_30d = sum(heat_deg_days, na.rm = TRUE),
            mean_max_temp_30d = mean(max_temp, na.rm = TRUE),
            mean_mean_temp_30d = mean(mean_temp, na.rm = TRUE),
            mean_min_temp_30d = mean(min_temp, na.rm = TRUE))

lakes_totalprecip_ndays_30days <- lakes_weather_30days %>%
  distinct(lakepulse_id, date, .keep_all = TRUE) %>%
  select(lakepulse_id, total_precip) %>%
  filter(!is.na(total_precip)) %>%
  group_by(lakepulse_id) %>%
  summarize(n_days = n())

# Add correction for total precipitation number of days
lakes_totalprecip_30days <- lakes_weather_30days %>%
  distinct(lakepulse_id, date, .keep_all = TRUE) %>%
  select(lakepulse_id, total_precip) %>%
  filter(!is.na(total_precip)) %>%
  group_by(lakepulse_id) %>%
  summarize(sum_total_precip_30d = sum(total_precip, na.rm = TRUE)) %>%
  left_join(lakes_totalprecip_ndays_30days, by = "lakepulse_id") %>%
  mutate(sum_total_precip_30d = sum_total_precip_30d/(n_days/30)) %>%
  select(-n_days)

lakes_weather_summary_30days <- lakes_weather_temp_30days %>%
  full_join(lakes_totalprecip_30days, by = "lakepulse_id")


#### Combine 7-day and 30-day weather data ####
lakes_weather_summary_all <- lakes_weather_summary_7days %>%
  full_join(lakes_weather_summary_30days, by = "lakepulse_id") %>%
  arrange(lakepulse_id)

# Write weather data to file
# lakes_weather_summary_all %>%
#   write_csv("data/curated/weather_7d30d_curated.csv", col_names = TRUE)
