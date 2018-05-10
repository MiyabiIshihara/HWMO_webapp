library(tidyverse)

## Load hazard data for data explorer
haz_tidy <- read_rds("data/tidy_haz.rds") %>%
  select(-c(AREA, PERIMETER, Acres, zone, CAR_Rating, CAR_Hawaii, CAR_adjtot))