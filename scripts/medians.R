# Fill in missing Lake Pulse environmental data

# Load libraries
library(tidyverse)
library(janitor)


#### Import curated environmental data ####
# Import most current environmental dataset
metadata <- read_csv("output/lakepulse_data_curated_2021-05-04.csv", col_names = TRUE)


#### Combine ecozones ####
# Combine Boreal Cordillera and Taiga Cordillera for the purpose of calculating ecozone medians
metadata <- metadata %>%
  mutate(ecozone_combined = case_when(ecozone == "Boreal Cordillera" | ecozone == "Taiga Cordillera" ~ "Boreal Cordillera/Taiga Cordillera",
                                      TRUE ~ ecozone))


#### Fill in missing chlorophyll-a AM data ####
metadata <- metadata %>%
  mutate(chla = case_when(is.na(chla_am) & !is.na(chla_pm) ~ chla_pm,
                          TRUE ~ chla_am)) %>%
  select(-chla_am, -chla_pm, -chla_day)


#### Calculate variable medians by ecozone ####
# No NA weather (ERA5 Land) data so no need to correct medians
# No NA basic info or land use data
metadata_medians <- metadata %>%
  select(lakepulse_id, ecozone_combined,
         volume, average_depth, discharge, residence_time, slope_100m,
         contains("rbr"),
         contains("do"),
         contains("ph"),
         chla, doc, dic, tss, colour,
         contains("spm"), contains("tp"), contains("tn"), contains("srp"),
         calcium, chloride, magnesium, potassium, sodium, sulfate,
         contains("secchi"),
         contains("bruntvaisala"), centerbuoyancy) %>%
  group_by(ecozone_combined) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)


#### Replace NA values by ecozone medians ####
for (i in 1:nrow(metadata)) {
  for (j in 1:ncol(metadata)) {
    if (colnames(metadata[i, j]) %in% colnames(metadata_medians[,-1]) & is.na(metadata[i, j])) {
      metadata[i, j] <- metadata_medians[which(metadata_medians$ecozone_combined == metadata$ecozone_combined[i]), colnames(metadata)[j]]
    }
  }
}


#### Write median-corrected environmental data to file ####
# Include date stamp
# Uncomment to write file:
# metadata %>%
#   select(-ecozone_combined) %>%
#   arrange(lakepulse_id) %>%
#   write_csv(paste0("output/median_corrected/lakepulse_data_curated_mediancorrected_", Sys.Date(), ".csv"), col_names = TRUE)
