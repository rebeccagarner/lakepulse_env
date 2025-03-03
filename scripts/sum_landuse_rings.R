# Calculate watershed area/fraction land use based on buffer rings

# Load libraries
library(tidyverse)


#### Import QC'ed land use rings data ####
landuse_rings <- read_delim("data/working/landuse/LakePulse_lulc_rings_simple_qc_20200901.csv", delim = ";", col_names = TRUE,
                            locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))


#### Format land use rings data ####
landuse_rings[landuse_rings == "Null"] <- NA

rings_area <- landuse_rings %>%
  select(lakepulse_id, ring, watershed_km2, contains("area")) %>%
  gather("var", "area", -lakepulse_id, -ring) %>%
  mutate(var = case_when(grepl("km2", var) ~ str_remove(var, "_km2"),
                         TRUE ~ var)) %>%
  mutate(var = case_when(grepl("_area", var) ~ str_remove(var, "_area"),
                         grepl("area_", var) ~ str_remove(var, "area_"),
                         TRUE ~ var)) %>%
  mutate(var = str_c("area_", var)) %>%
  mutate(area = as.numeric(area)) %>%
  mutate(ring = str_replace(ring, "Ring[:space:]", "ring")) %>%
  mutate(ring = str_replace(ring, "[:space:]m", "m")) %>%
  filter(!is.na(area))

# Extract watershed area totals
watershed_area <- rings_area %>%
  filter(var == "area_watershed") %>%
  pivot_wider(names_from = var, values_from = area, values_fill = NA) %>%
  select(lakepulse_id, area_watershed) %>%
  distinct(lakepulse_id, .keep_all = TRUE)

# Sum ring areas starting from the shoreline
rings_area_cumulative <- rings_area %>%
  filter(var != "area_watershed") %>%
  pivot_wider(names_from = ring, values_from = area, values_fill = NA) %>%
  mutate(`ring0-100m` = case_when(!is.na(ring0m) & !is.na(`ring0-100m`) ~ ring0m + `ring0-100m`)) %>%
  mutate(`ring0-300m` = case_when(!is.na(`ring0-100m`) & !is.na(`ring100-300m`) ~ `ring0-100m` + `ring100-300m`)) %>%
  mutate(`ring0-700m` = case_when(!is.na(`ring0-300m`) & !is.na(`ring300-700m`) ~ `ring0-300m` + `ring300-700m`)) %>%
  mutate(`ring0-1500m` = case_when(!is.na(`ring0-700m`) & !is.na(`ring700-1500m`) ~ `ring0-700m` + `ring700-1500m`)) %>%
  mutate(`ring0-3100m` = case_when(!is.na(`ring0-1500m`) & !is.na(`ring1500-3100m`) ~ `ring0-1500m` + `ring1500-3100m`)) %>%
  mutate(`ring0-6300m` = case_when(!is.na(`ring0-3100m`) & !is.na(`ring3100-6300m`) ~ `ring0-3100m` + `ring3100-6300m`)) %>%
  mutate(`ring0m-end` = case_when(!is.na(`ring0-6300m`) & !is.na(`ring6300m-end`) ~ `ring0-6300m` + `ring6300m-end`)) %>%
  select(lakepulse_id, var, contains("ring0")) %>%
  pivot_longer(cols = contains("ring"), names_to = "ring", values_to = "area") %>%
  filter(!is.na(area))

# Extract ring area totals
rings_area_total <- rings_area_cumulative %>%
  filter(var == "area_total") %>%
  pivot_wider(names_from = var, values_from = area, values_fill = NA)

# Derive cumulative fractions of watershed and fractions of ring
rings_fraction_cumulative <- rings_area_cumulative %>%
  left_join(watershed_area, by = "lakepulse_id") %>%
  left_join(rings_area_total, by = c("lakepulse_id", "ring")) %>%
  mutate(fraction_watershed = area/area_watershed,
         fraction_ring = area/area_total) %>%
  select(lakepulse_id, var, ring, contains("fraction"))

# Combine all cumulative ring land use data
rings_all <- rings_area_cumulative %>%
  filter(var != "area_na") %>%
  filter(var != "area_total") %>%
  left_join(rings_fraction_cumulative, by = c("lakepulse_id", "var", "ring")) %>%
  mutate(var = str_remove(var, "area_")) %>%
  gather("calc", "value", -lakepulse_id, -var, -ring) %>%
  pivot_wider(names_from = c(calc, var, ring), values_from = value, names_sep = "_", values_fill = NA)


#### Write cumulative ring land use data to file ####
# rings_all %>%
#   write_csv("data/curated/landuse_rings_cumulative_curated.csv", col_names = TRUE)
