# Curate bedrock geology data

# Load libraries
library(tidyverse)
library(readxl)
library(janitor)


#### Import bedrock geology data ####
# Import general rock category data
rxtp <- read_xlsx("data/working/LakePulse_BedrockGeology_qc_20200901.xlsx", sheet = "RXTP", col_names = TRUE, skip = 1) %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse) %>%
  select(-wshd_area_km2)

# Import specific rock category data
subrxtp <- read_xlsx("data/working/LakePulse_BedrockGeology_qc_20200901.xlsx", sheet = "SUBRXTP", col_names = TRUE, skip = 1) %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse) %>%
  select(-wshd_area_km2)

# Import age/rock type combination data
agextp <- read_xlsx("data/working/LakePulse_BedrockGeology_qc_20200901.xlsx", sheet = "AGERXTP", col_names = TRUE, skip = 1) %>%
  clean_names() %>%
  rename(lakepulse_id = id_lake_pulse) %>%
  select(-wshd_area_km2)


#### Select relevant variables ####
# Relevant variables were identified by Katie Griffiths
# Subset carbonitic rocks
carbonitic <- subrxtp %>%
  select(lakepulse_id, contains(c("carbonate", "marble", "offshelf_miogeoclinal"))) %>%
  mutate(rock_type = "carbonitic")

# Subset evaporites
evaporites <- subrxtp %>%
  select(lakepulse_id, contains(c("evaporite"))) %>%
  mutate(rock_type = "evaporites")

# Subset potential carbonate rocks
potential_carbonates <- subrxtp %>%
  select(lakepulse_id, contains(c("sedimentary_and_mafic_volcanic", "sedimentary_and_volcanic_gneiss", "undivided_sedimentary"))) %>%
  mutate(rock_type = "potential_carbonates")


#### Sum surface areas/fractions in watersheds by rock type ####
# Define function to calculate sums by rock type
sumRocks <- function(data) {
  output <- data %>%
    pivot_longer(!c(lakepulse_id, rock_type), names_to = "var", values_to = "value") %>%
    mutate(var_type = case_when(grepl("km2", var) ~ "area",
                                grepl("frac", var) ~ "fraction")) %>%
    group_by(lakepulse_id, rock_type, var_type) %>%
    summarize(sum_value = sum(value)) %>%
    pivot_wider(names_from = c(var_type, rock_type), values_from = sum_value, names_sep = "_")
    #pivot_wider(names_from = var_type, values_from = sum_value)
  return(output)
}

# Check that all rock types in a category sum to fraction = 1
sumRocks(subrxtp %>%
           mutate(rock_type = "all"))

# Sum areas/fractions by rock type
(carbonitic_total <- sumRocks(carbonitic))
(evaporites_total <- sumRocks(evaporites))
(potential_carbonates_total <- sumRocks(potential_carbonates))


#### Combine relevant bedrock geology variables ####
# Join and reorder data
geology_curated <- carbonitic_total %>%
  left_join(evaporites_total, by = "lakepulse_id") %>%
  left_join(potential_carbonates_total, by = "lakepulse_id") %>%
  select(lakepulse_id, contains("area"), contains("fraction")) %>%
  arrange(lakepulse_id)

# Write curated bedrock geology data to file
# geology_curated %>%
#   write_csv("data/curated/bedrock_geology_curated.csv", col_names = TRUE)
