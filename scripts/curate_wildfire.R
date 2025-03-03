# Curate wildfire data

# Load libraries
library(tidyverse)
library(readxl)
library(janitor)

#### Import five-year wildfire data ####
# Define function to read and format wildfire data files
readWildfire <- function(file) {
  h1 <- file %>%
    read_xlsx(sheet = 1) %>%
    names()
  
  h2 <- file %>%
    read_xlsx(sheet = 1, skip = 1) %>%
    names()
  
  headers <- tibble(headers1 = h1, headers2 = h2) %>%
    mutate(headers1 = case_when(headers1 == "idLakePulse" ~ "lakepulse_id",
                                grepl("\\.\\.\\.", headers1) ~ "",
                                TRUE ~ headers1),
           headers2 = str_remove(headers2, "\\.\\.\\..*"))
  for (i in 1:nrow(headers)) {
    if (headers$headers1[i] == "") {
      headers$headers1[i] <- headers$headers1[i-1]
    }
  }
  
  new_headers <- headers %>%
    mutate(newheader = case_when(headers2 == "" ~ headers1,
                                 TRUE ~ str_c(headers2, headers1, sep = "_"))) %>%
    pull(newheader)
  
  wildfire <- read_xlsx(file,
                        sheet = 1, col_names = new_headers, skip = 2) %>%
    clean_names() %>%
    select(-wshd_area_km2) %>%
    pivot_longer(!lakepulse_id, names_to = "var", values_to = "value") %>%
    mutate(var = str_remove(var, "wildfires_")) %>%
    separate(var, into = c("var", "year"), sep = "_") %>%
    mutate(var = case_when(var == "percent" ~ "fraction",
                           var == "km2" ~ "area"),
           year = as.numeric(year)) %>%
    mutate(year_from_sampling = max(year) - year) %>%
    mutate(header = str_c(var, "_fire_minus", year_from_sampling, "yr", sep = "")) %>%
    select(lakepulse_id, header, value) %>%
    pivot_wider(names_from = header, values_from = value)
  
  return(wildfire)
}

wildfire2017 <- readWildfire("data/working/wildfires/calculateWildfiresProportion_2017sampledLakes.xlsx")
wildfire2018 <- readWildfire("data/working/wildfires/calculateWildfiresProportion_2018sampledLakes.xlsx")
wildfire2019 <- readWildfire("data/working/wildfires/calculateWildfiresProportion_2019sampledLakes.xlsx")


#### Combine data from three years of sampling ####
wildfire_all <- bind_rows(wildfire2017,
                          wildfire2018,
                          wildfire2019) %>%
  arrange(lakepulse_id)


#### Derive multi-year cumulative watershed burn variables ####
wildfire_all <- wildfire_all %>%
  mutate(fraction_fire_6yrs = fraction_fire_minus0yr + fraction_fire_minus1yr + fraction_fire_minus2yr + fraction_fire_minus3yr + fraction_fire_minus4yr + fraction_fire_minus5yr,
         area_fire_6yrs = area_fire_minus0yr + area_fire_minus1yr + area_fire_minus2yr + area_fire_minus3yr + area_fire_minus4yr + area_fire_minus5yr)

# Write curated wildfire data to file
# wildfire_all %>%
#   write_csv("data/curated/wildfire_curated.csv", col_names = TRUE)
