# Curate ERA-5 Land weather data

# Load libraries
library(tidyverse)
library(janitor)


#### Import and format QC'ed data ####
# Import mean temperature data
temperature_7d <- read_tsv("data/working/era5land/2m_temperature_processing7daysMeans.txt") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse,
         temperature_mean_7d = x2m_temperature_c)

temperature_30d <- read_tsv("data/working/era5land/2m_temperature_processing30daysMeans.txt") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse,
         temperature_mean_30d = x2m_temperature_c)

temperature_30d_daily <- read_tsv("data/working/era5land/2m_temperature_processingDailyMeansOver30days.txt", comment = "#") %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse)

# Import total precipitation data
precipitation <- read_delim("data/working/era5land/LakePulse_climate_data_total_precipitation_20220406.csv", delim = ";", col_names = TRUE,
                            locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8")) %>%
  dplyr::select(lakepulse_id, starts_with("sum")) %>%
  rename(precipitation_total_7d = sum7daysPrior,
         precipitation_total_30d = sum30daysPrior)

# Import net solar radiation data
radiation <- read_delim("data/working/era5land/LakePulse_climate_data_solar_radiation_20220406.csv", delim = ";", col_names = TRUE,
                        locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8")) %>%
  dplyr::select(lakepulse_id, starts_with("mean")) %>%
  rename(solar_radiation_net_7d = mean7daysPrior,
         solar_radiation_net_30d = mean30daysPrior)

# Import wind speed data
wind <- read_delim("data/working/era5land/LakePulse_climate_data_10m_wind_20210201.csv", col_names = TRUE,
                   delim = ";") %>%
  select(lakepulse_id, mean7daysPrior, mean30daysPrior) %>%
  rename(windspeed_mean_7d = mean7daysPrior,
         windspeed_mean_30d = mean30daysPrior)


#### Calculate degree-days based on 18 degC threshold ####
# Heating degree-days for a given day are the number of degrees Celsius that the
# mean temperature is below 18 °C. If the temperature is equal to or greater than
# 18 °C, then the number will be zero. For example, a day with a mean temperature
# of 15.5 °C has 2.5 heating degree-days; a day with a mean temperature of 20.5 °C
# has zero heating degree-days.

# Cooling degree-days for a given day are the number of degrees Celsius that the
# mean temperature is above 18 °C. If the temperature is equal to or less than
# 18 °C, then the number will be zero. For example, a day with a mean temperature
# of 20.5 °C has 2.5 cooling degree-days; a day with a mean temperature of 15.5 °C
# has zero cooling degree-days.

degreeday_threshold <- 18

degdays <- temperature_30d_daily %>%
  select(-sampling_date) %>%
  pivot_longer(!lakepulse_id, names_to = "minus_ndays", values_to = "temperature") %>%
  mutate(minus_ndays = str_remove(minus_ndays, "daily_mean_day_")) %>%
  mutate(minus_ndays = as.numeric(str_remove(minus_ndays, "_before_sampling"))) %>%
  mutate(heating_degdays = case_when(temperature >= degreeday_threshold ~ 0,
                                     temperature < degreeday_threshold ~ degreeday_threshold - temperature),
         cooling_degdays = case_when(temperature >= degreeday_threshold ~ temperature - degreeday_threshold,
                                     temperature < degreeday_threshold ~ 0))

degdays_30d <- degdays %>%
  group_by(lakepulse_id) %>%
  summarize(heating_degdays_mean_30d = mean(heating_degdays),
            heating_degdays_total_30d = sum(heating_degdays),
            cooling_degdays_mean_30d = mean(cooling_degdays),
            cooling_degdays_total_30d = sum(cooling_degdays))

degdays_7d <- degdays %>%
  filter(minus_ndays <= 7) %>%
  group_by(lakepulse_id) %>%
  summarize(heating_degdays_mean_7d = mean(heating_degdays),
            heating_degdays_total_7d = sum(heating_degdays),
            cooling_degdays_mean_7d = mean(cooling_degdays),
            cooling_degdays_total_7d = sum(cooling_degdays))


#### Combine data ####
weather_curated <- temperature_7d %>%
  left_join(temperature_30d, by = "lakepulse_id") %>%
  left_join(precipitation, by = "lakepulse_id") %>%
  left_join(radiation, by = "lakepulse_id") %>%
  left_join(wind, by = "lakepulse_id") %>%
  left_join(degdays_7d, by = "lakepulse_id") %>%
  left_join(degdays_30d, by = "lakepulse_id") %>%
  arrange(lakepulse_id)


#### Write curated data to file ####
# weather_curated %>%
#   write_csv("data/curated/weather_era5land_curated.csv", col_names = TRUE)
