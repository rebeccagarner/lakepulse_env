# Curate QC'ed Lake Pulse total phosphorus (TP) data

# Load libraries
library(tidyverse)
library(janitor)


#### Import QC'ed data ####
# Remove NA TP concentrations
tp <- read_delim("data/working/LakePulse_tp_qc_20201116.csv", delim = ";", col_names = TRUE,
                 locale = locale(decimal_mark = ".", grouping_mark = ";", encoding = "UTF-8")) %>%
  filter(!is.na(tp_concentration))


#### Visualize data ####
tp %>%
  ggplot(aes(tp_concentration)) +
  geom_histogram(binwidth = 100) +
  facet_grid(~depth) +
  theme_bw()


#### Filter data ####
# Remove data with comment "MPV: TP result is clearly incompatible with other results (chl a, SRP, dissolved P, POM-P), we recommend not using this datum"
tp <- tp %>%
  filter(!grepl("we recommend not using this datum", tp_comment))

# If CV over 50 (flag 3) then make NA?
# No, because deviates further from real TP than ecozone depth median TP
# tp_rmflag3 <- tp %>%
#   filter(cv < 50)


#### Format data ####
tp_curated <- tp %>%
  select(lakepulse_id, depth, tp_concentration) %>%
  pivot_wider(names_from = depth, values_from = tp_concentration, values_fill = NA) %>%
  rename(tp_epilimnion = Epilimnion,
         tp_hypolimnion = Hypolimnion)

# Write curated data to file
# tp_curated %>%
#   write_csv("data/curated/tp_curated.csv", col_names = TRUE)


#### Assess impact of removing TP with flag 3 ####
# tp_medians <- tp %>%
#   select(lakepulse_id, depth, tp_concentration) %>%
#   left_join(ecozones, by = "lakepulse_id") %>%
#   group_by(depth, ecozone) %>%
#   summarise_if(is.numeric, median, na.rm = TRUE) %>%
#   rename(tp = tp_concentration)
# 
# tp_rmflag3_medians <- tp_rmflag3 %>%
#   select(lakepulse_id, depth, tp_concentration) %>%
#   left_join(ecozones, by = "lakepulse_id") %>%
#   group_by(depth, ecozone) %>%
#   summarise_if(is.numeric, median, na.rm = TRUE) %>%
#   rename(tp_rmflag3 = tp_concentration)
# 
# tp_medians_all <- tp_medians %>%
#   full_join(tp_rmflag3_medians, by = c("depth", "ecozone"))
# 
# tp_cv50plus <- tp %>%
#   filter(cv >= 50) %>%
#   select(lakepulse_id, depth, tp_concentration)
# 
# tp_cv50plus %>%
#   left_join(ecozones, by = "lakepulse_id") %>%
#   left_join(tp_medians_all, by = c("depth", "ecozone"))
