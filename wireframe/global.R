library(tidyverse)

## Load hazard data for data explorer
haz_tidy <- read_csv("data/tidy_haz.csv") %>%
  select(-c(AREA, PERIMETER, Acres, zone, CAR_Hawaii, CAR_adjtot))