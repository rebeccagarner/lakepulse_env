# Categorize and combine curated LakePulse data

# load libraries
library(tidyverse)
library(janitor)
library(lubridate)


#### Import data files ####
(basicinfo_landuse <- read_csv("data/curated/basicinfo_landuse_curated.csv", col_names = TRUE))
(circularity <- read_csv("data/curated/circularity_curated.csv"))
(hydrolakes <- read_csv("data/curated/lakePulse_hydrolake_20200901_QC.csv", col_names = TRUE))
(bedrock_geology <- read_csv("data/curated/bedrock_geology_curated.csv", col_names = TRUE))

(rbr_chla <- read_csv("data/curated/chla_rbr_curated.csv", col_names = TRUE))
(rbr_do <- read_csv("data/curated/DO_rbr_curated.csv", col_names = TRUE))
(rbr_salinity <- read_csv("data/curated/Salinity_curated.csv", col_names = TRUE))
(rbr_conductivity <- read_csv("data/curated/Conductivity_curated.csv", col_names = TRUE))
(rbr_specificconductance <- read_csv("data/curated/Specific conductance_curated.csv", col_names = TRUE))
(rbr_seapressure <- read_csv("data/curated/sea_pressure_rbr_curated.csv", col_names = TRUE))
(rbr_soundvelocity <- read_csv("data/curated/sound_velocity_rbr_curated.csv", col_names = TRUE))
(rbr_temp <- read_csv("data/curated/temperature_rbr_curated.csv", col_names = TRUE))
(rbr_temp_means <- read_csv("data/curated/rbr_temperature_means.csv", col_names = TRUE))

(chla <- read_csv("data/curated/LPcuration_ChlorophyllFilter_DailyAverage_012021_MB.csv", col_names = TRUE))
(dicdoc_tss <- read_csv("data/curated/LPcuration_DOC_DIC_TSS_082020_MB.csv", col_names = TRUE))
(ph <- read_csv("data/curated/ph_curated.csv", col_names = TRUE))
(tp <- read_csv("data/curated/tp_curated.csv", col_names = TRUE))
(tn <- read_csv("data/curated/TN_curated.csv", col_names = TRUE))
(srp <- read_csv("data/curated/SRP_final.csv", col_names = TRUE))
(ions <- read_csv("data/curated/ions_curated.csv", col_names = TRUE))
(colour <- read_csv("data/curated/colour.csv", col_names = TRUE))
(nox <- read_csv("data/curated/nox_curated.csv", col_names = TRUE))

(secchi <- read_csv("data/curated/secchi_curated.csv", col_names = TRUE))
(kestrel <- read_csv("data/curated/kestrel_curated.csv", col_names = TRUE))

(bruntvaisala <- read_csv("data/curated/Brunt-Vaisala_curated.csv", col_names = TRUE))

(weather <- read_csv("data/curated/weather_era5land_curated.csv", col_names = TRUE))
(ice_disappearance <- read_csv("data/curated/ice_dissapearence_date_20210201.csv", col_names = TRUE))

(human_population <- read_csv("data/curated/human_population_curated.csv", col_names = TRUE))

(wildfire <- read_csv("data/curated/wildfire_curated.csv", col_names = TRUE))


#### Format curated data ####
(basicinfo_landuse <- basicinfo_landuse %>%
   rename(area_natlandscapes = area_nat_landscapes,
          fraction_natlandscapes = fraction_nat_landscapes) %>%
   mutate(max_depth = case_when((max_depth_found >= max_depth_bathymetric_map) |
                                  (!is.na(max_depth_found) & is.na(max_depth_bathymetric_map)) ~ max_depth_found,
                                TRUE ~ max_depth_bathymetric_map)) %>%
   select(-max_depth_found, -max_depth_bathymetric_map))

(hydrolakes <- hydrolakes %>%
    select(lakepulse_id, vol_total, depth_avg, dis_avg, res_time, slope_100, pour_long, pour_lat) %>%
    rename(volume = vol_total,
           average_depth = depth_avg,
           discharge = dis_avg,
           residence_time = res_time,
           slope_100m = slope_100,
           pour_longitude = pour_long,
           pour_latitude = pour_lat))
hydrolakes$residence_time[hydrolakes$residence_time == -1] <- NA

(rbr_chla <- rbr_chla %>%
    rename(rbr_chla_mean_bottom = chla_bottom,
           rbr_chla_mean_tube = chla_tube) %>%
    select(lakepulse_id, rbr_chla_mean_tube, rbr_chla_mean_bottom))

(rbr_do <- rbr_do %>%
    rename(do_percenttsaturation_bottom = do_mean_bottom,
           do_percenttsaturation_tube = do_mean_tube))

(rbr_salinity <- rbr_salinity %>%
    rename(rbr_salinity_tube = salinity_tube_length_CP,
           rbr_salinity_bottom = salinity_mean_bottom,
           rbr_salinity_hypolimnion = salinity_mean_hypo))

(rbr_conductivity <- rbr_conductivity %>%
    rename(rbr_conductivity_tube = conductivity_tube_length,
           rbr_conductivity_bottom = conductivity_mean_bottom,
           rbr_conductivity_hypolimnion = conductivity_mean_hypo))

(rbr_specificconductance <- rbr_specificconductance %>%
    rename(rbr_specific_conductance_tube = specific_conductance_tube_length,
           rbr_specific_conductance_bottom = specific_conductance_mean_bottom,
           rbr_specific_conductance_hypolimnion = specific_conductance_hypolimnetic))

(rbr_seapressure <- rbr_seapressure %>%
    rename(rbr_sea_pressure_mean_bottom = sea_pressure_bottom,
           rbr_sea_pressure_mean_tube = sea_pressure_tube) %>%
    select(lakepulse_id, rbr_sea_pressure_mean_tube, rbr_sea_pressure_mean_bottom))

(rbr_soundvelocity <- rbr_soundvelocity %>%
    rename(rbr_sound_velocity_mean_bottom = sound_velocity_bottom,
           rbr_sound_velocity_mean_tube = sound_velocity_tube) %>%
    select(lakepulse_id, rbr_sound_velocity_mean_tube, rbr_sound_velocity_mean_bottom))

(rbr_temp <- rbr_temp %>%
    rename(rbr_temperature_mean_bottom = temperature_bottom,
           rbr_temperature_mean_tube = temperature_tube) %>%
    select(lakepulse_id, rbr_temperature_mean_tube, rbr_temperature_mean_bottom))

(rbr_temp_means <- rbr_temp_means %>%
    rename(lakepulse_id = lake_id) %>%
    select(lakepulse_id, rbr_temperature_mean_watercolumn, rbr_temperature_mean_top0.5m,
           rbr_temperature_mean_top1m, rbr_temperature_mean_topmost,
           rbr_temperature_mean_epilimnion, rbr_temperature_mean_hypolimnion,
           stratification))

(rbr_temp <- rbr_temp %>%
    left_join(rbr_temp_means, by = "lakepulse_id"))

# Fill in missing water column temperature data
(rbr_temp <- rbr_temp %>%
    mutate(rbr_temperature_mean_top0.5m = case_when(is.na(rbr_temperature_mean_top0.5m) & !is.na(rbr_temperature_mean_tube) ~ rbr_temperature_mean_tube,
                                                    TRUE ~ rbr_temperature_mean_top0.5m),
           rbr_temperature_mean_top1m = case_when(is.na(rbr_temperature_mean_top1m) & !is.na(rbr_temperature_mean_tube) ~ rbr_temperature_mean_tube,
                                                  TRUE ~ rbr_temperature_mean_top1m),
           rbr_temperature_mean_epilimnion = case_when(is.na(rbr_temperature_mean_epilimnion) & !is.na(rbr_temperature_mean_tube) ~ rbr_temperature_mean_tube,
                                                       TRUE ~ rbr_temperature_mean_epilimnion),
           rbr_temperature_mean_hypolimnion = case_when(is.na(rbr_temperature_mean_hypolimnion) & !is.na(rbr_temperature_mean_bottom) ~ rbr_temperature_mean_bottom,
                                                        TRUE ~ rbr_temperature_mean_hypolimnion),
           rbr_temperature_mean_bottom = case_when(is.na(rbr_temperature_mean_bottom) & !is.na(rbr_temperature_mean_hypolimnion) ~ rbr_temperature_mean_hypolimnion,
                                                   TRUE ~ rbr_temperature_mean_bottom)))

(chla <- chla %>%
    select(-X1, -contains("stdev")) %>%
    clean_names())

(dicdoc_tss <- dicdoc_tss %>%
    select(-X1, -contains(c("cv", "flag"))) %>%
    rename(spm_mineral = mineral_spm,
           spm_organic = organic_spm))

(tn <- tn %>%
    rename(tn_bottom = tn_bottom_m,
           tn_tube = tn_tube_CP) %>%
    select(lakepulse_id, tn_tube, tn_bottom))
tn$tn_tube[which(tn$tn_tube > 6)] <- NA
tn$tn_bottom[which(tn$tn_bottom > 6)] <- NA

(tp <- tp %>%
    rename(tp_tube = tp_epilimnion,
           tp_bottom = tp_hypolimnion))

(tntp_ratio <- tp %>%
    left_join(tn, by = "lakepulse_id") %>%
    mutate(tntp_ratio = tn_tube*1000/tp_tube) %>%
    select(lakepulse_id, tntp_ratio))

(srp <- srp %>%
    rename(srp_epilimnion = concentration_epi,
           srp_hypolimnion = concentration_hypo))

(colour <- colour %>%
    rename(colour = `Color (Pt, mg liter-1)`))
range(colour$colour, na.rm = TRUE)  # Check that there are no negative values

secchi[secchi == "no data"] <- NA
(secchi <- secchi %>%
    rename(secchi_pm_optics = mean_pm_optics) %>%
    mutate(secchi_pm_optics = as.numeric(secchi_pm_optics)))

(kestrel <- kestrel %>%
    rename(air_temperature_am_index = temperature_am_index,
           air_temperature_pm_optics = temperature_pm_optics,
           relative_humidity_am_index = rel_humid_am_index,
           relative_humidity_pm_optics = rel_humid_pm_optics,
           atm_pressure_am_index = atm_press_am_index,
           atm_pressure_pm_optics = atm_press_pm_optics,
           wind_speed_max_am_index = wind_max_am_index,
           wind_speed_max_pm_optics = wind_max_pm_optics,
           wind_speed_avg_am_index = wind_avg_am_index,
           wind_speed_avg_pm_optics = wind_avg_pm_optics))

(bruntvaisala <- bruntvaisala %>%
    rename(bruntvaisala_max = BruntVaisala_max,
           bruntvaisala_mean = BruntVaisala_mean,
           centerbuoyancy = CenterBuoyancy) %>%
    select(lakepulse_id, bruntvaisala_max, bruntvaisala_mean, centerbuoyancy))

(ice_disappearance <- ice_disappearance %>%
    mutate(ice_disappearance_julianday = lubridate::yday(iceDissapearenceDate)) %>%
    mutate(ice_disappearance_julianday = case_when(is.na(ice_disappearance_julianday) ~ 0,
                                                   TRUE ~ ice_disappearance_julianday)) %>%
    select(lakepulse_id, ice_disappearance_julianday))

(wildfire <- wildfire %>%
    select(lakepulse_id, fraction_fire_minus0yr, area_fire_minus0yr,
           fraction_fire_6yrs, area_fire_6yrs) %>%
    rename(fraction_fire = fraction_fire_minus0yr,
           area_fire = area_fire_minus0yr))


#### Combine data ####
curated <- basicinfo_landuse %>%
  left_join(circularity, by = "lakepulse_id") %>%
  left_join(hydrolakes, by = "lakepulse_id") %>%
  left_join(bedrock_geology, by = "lakepulse_id") %>%
  left_join(rbr_chla, by = "lakepulse_id") %>%
  left_join(rbr_do, by = "lakepulse_id") %>%
  left_join(rbr_salinity, by = "lakepulse_id") %>%
  left_join(rbr_conductivity, by = "lakepulse_id") %>%
  left_join(rbr_specificconductance, by = "lakepulse_id") %>%
  left_join(rbr_seapressure, by = "lakepulse_id") %>%
  left_join(rbr_soundvelocity, by = "lakepulse_id") %>%
  left_join(rbr_temp, by = "lakepulse_id") %>%
  left_join(chla, by = "lakepulse_id") %>%
  left_join(dicdoc_tss, by = "lakepulse_id") %>%
  left_join(ph, by = "lakepulse_id") %>%
  left_join(tp, by = "lakepulse_id") %>%
  left_join(tn, by = "lakepulse_id") %>%
  left_join(tntp_ratio, by = "lakepulse_id") %>%
  left_join(srp, by = "lakepulse_id") %>%
  left_join(colour, by = "lakepulse_id") %>%
  left_join(nox, by = "lakepulse_id") %>%
  left_join(ions, by = "lakepulse_id") %>%
  left_join(secchi, by = "lakepulse_id") %>%
  left_join(kestrel, by = "lakepulse_id") %>%
  left_join(bruntvaisala, by = "lakepulse_id") %>%
  left_join(weather, by = "lakepulse_id") %>%
  left_join(ice_disappearance, by = "lakepulse_id") %>%
  left_join(human_population, by = "lakepulse_id") %>%
  left_join(wildfire, by = "lakepulse_id") %>%
  arrange(lakepulse_id)


#### Write data table to file ####
# Include date stamp
# Uncomment to write file:
# curated %>%
#   write_csv(paste0("output/lakepulse_data_curated_", Sys.Date(), ".csv"), col_names = TRUE)
