# Curate lake circularity data
# The circularity index was calculated by dividing the area by the square of the
# perimeter of the polygon and that value was multiplied by 4*pi.

# Load libraries
library(tidyverse)
library(janitor)


#### Import and format lake circularity data ####
circularity <- read_csv("data/working/LakePulse_circularity.csv", col_names = TRUE) %>%
  rename(lakepulse_id = idLakePulse) %>%
  clean_names() %>%
  arrange(lakepulse_id)

circularity <- circularity %>%
  select(lakepulse_id, circularity)


#### Write curated lake circularity data to file ####
# circularity %>%
#   write_csv("data/curated/circularity_curated.csv", col_names = TRUE)
