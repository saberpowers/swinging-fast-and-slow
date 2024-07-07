# PURPOSE: Gather initial data from the season so far (5/22/24)

library(tidyverse)
source("sandbox/ryurko/download_savant_data.R")

# Gather the data ---------------------------------------------------------

START_DATE <- "2024-03-20"
END_DATE <- "2024-05-21"

current_season_data <- download_baseballsavant(START_DATE, END_DATE)

# Filter to bat tracking data and save ------------------------------------

bat_tracking_data <- current_season_data |>
  filter(!is.na(bat_speed), !is.na(swing_length))

write_rds(bat_tracking_data, "data/bat_tracking_0705.rds")
