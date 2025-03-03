# Curate QC'ed Lake Pulse pH data from multiple sources

# Load libraries
library(tidyverse)
library(janitor)


#### Import QC'ed data ####
ph <- read_delim("data/working/LakePulse_ph_qc_20210315.csv", delim = ";", col_names = TRUE,
                 locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

lakes <- ph %>%
  distinct(lakepulse_id)


#### Filter data ####
# Remove missing data
ph <- ph %>%
  filter(!is.na(ph))

# Remove data flagged -1
ph <- ph %>%
  filter(!grepl("-1", ph_flags))


#### Format data ####
ph_curated <- ph %>%
  select(lakepulse_id, depth, ph) %>%
  pivot_wider(names_from = depth, values_from = ph, values_fill = NA) %>%
  rename(ph_epilimnion = Epilimnion,
         ph_hypolimnion = Hypolimnion)

ph_curated <- lakes %>%
  left_join(ph_curated, by = "lakepulse_id") %>%
  arrange(lakepulse_id)

# Write curated data to file
# ph_curated %>%
#   write_csv("data/curated/ph_curated.csv", col_names = TRUE)
