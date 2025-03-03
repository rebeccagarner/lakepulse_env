# Curate QC'ed Lake Pulse basic info and land use data

# Load libraries
library(tidyverse)
library(janitor)
#library(lubridate)


#### Import QC'ed data ####
# Basic info
basic_info <- read_delim("data/working/basic_info/LakePulse_basic_info_qc_20201015.csv", delim = ";", col_names = TRUE,
                         locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

depth <- read_delim("data/working/basic_info/LakePulse_basic_info_depth_qc_20200901.csv", delim = ";", col_names = TRUE,
                    locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

watershed <- read_delim("data/working/basic_info/LakePulse_watershedarea_qc_20200901.csv", delim = ";", col_names = TRUE,
                        locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

# Land use
landuse <- read_delim("data/working/landuse/LakePulse_lulc_simple_qc_20200901.csv", delim = ";", col_names = TRUE,
                      locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))

landuse_rings <- read_delim("data/working/landuse/LakePulse_lulc_rings_simple_qc_20200901.csv", delim = ";", col_names = TRUE,
                            locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8"))


#### Verify and format data ####
basic_info <- basic_info %>%
  select(-team, -flags, -comment) %>%
  rename(shoreline_length = shorline_length)

# Extract max depth recorded in bathymetric map
depth <- depth %>%
  mutate(max_depth_bathymetric_map = case_when(grepl("bathymetric map", comment_depth_found, ignore.case = TRUE) & grepl("\\(max depth", comment_depth_found) ~ str_remove(comment_depth_found, ".*\\(max depth[:space:]"))) %>%
  mutate(max_depth_bathymetric_map = as.numeric(str_remove(max_depth_bathymetric_map, "m.*"))) %>%
  select(lakepulse_id, max_depth_found, max_depth_bathymetric_map)

watershed <- watershed %>%
  select(-idLatLong) %>%
  rename(watershed_area = watershed_km2)

# Check that land use variables balance
# 1) Watershed land use areas should equal the watershed surface areas
# 2) Watershed land use fractions should sum to 1
landuse_vars <- landuse %>%
  select(-flags, -comment, -total_km2_area) %>%
  gather("var", "value", -lakepulse_id) %>%
  mutate(var_type = case_when(grepl("area", var) ~ "area",
                              grepl("fraction", var) ~ "fraction")) %>%
  group_by(lakepulse_id, var_type) %>%
  summarize(sum_type = sum(value))

landuse_vars %>%
  filter(var_type == "area") %>%
  left_join(watershed, by = "lakepulse_id") %>%
  mutate(area_agree = case_when(round(sum_type, 1) == round(watershed_area, 1) ~ TRUE,
                                TRUE ~ FALSE))  # All component areas sum to the watershed area

landuse_vars %>%
  filter(var_type == "fraction")  # All fractions sum to 1

landuse <- landuse %>%
  select(-flags, -comment, -total_km2_area) %>%
  select(-area_km2_na, -fraction_na) %>%
  gather("var", "value", -lakepulse_id) %>%
  mutate(var = case_when(grepl("km2", var) ~ str_remove(var, "_km2"),
                         TRUE ~ var)) %>%
  spread(var, value)

landuse_rings[landuse_rings == "Null"] <- NA
landuse_rings <- landuse_rings %>%
  select(-flags, -comment) %>%
  select(-watershed_km2, -area_km2_na, -fraction_na) %>%
  gather("var", "value", -lakepulse_id, -ring) %>%
  mutate(var = case_when(grepl("km2", var) ~ str_remove(var, "_km2"),
                         TRUE ~ var)) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(ring = str_replace(ring, "Ring[:space:]", "ring")) %>%
  mutate(ring = str_replace(ring, "[:space:]m", "m")) %>%
  pivot_wider(names_from = c(var, ring), values_from = value, names_sep = "_")


#### Combine basic info and land use data ####
basicinfo_landuse <- basic_info %>%
  left_join(depth, by = "lakepulse_id") %>%
  left_join(watershed, by = "lakepulse_id") %>%
  left_join(landuse, by = "lakepulse_id")


#### Write curated data to file ####
# basicinfo_landuse %>%
#   write_csv("data/curated/basicinfo_landuse_curated.csv", col_names = TRUE)


#### Write separate file for curated land use rings data ####
# landuse_rings %>%
#   write_csv("data/curated/landuse_rings_curated.csv", col_names = TRUE)
