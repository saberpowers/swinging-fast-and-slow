# PURPOSE: Find the primary type of fastball for every pitcher in the dataset,
#          then save this table for future use

library(tidyverse)

# Load the data -----------------------------------------------------------

full_swing_data <- read_rds("data/bat_tracking_2024_season.rds")
  
# Filter to fastballs -----------------------------------------------------

fastball_data <- full_swing_data |>
  filter(pitch_type %in% c("FF", "SI", "FC"))

# Find primary fastball for each pitcher ----------------------------------

pitcher_fastball_type <- fastball_data |>
  group_by(pitcher_id, pitch_type) |>
  count() |>
  ungroup() |>
  group_by(pitcher_id) |>
  slice_max(order_by = `n`, n = 1, with_ties = FALSE) |>
  ungroup()

# Save this table ---------------------------------------------------------

pitcher_fastball_type |>
  dplyr::select(-n) |>
  rename(primary_fastball = pitch_type) |>
  write_csv("data/primary_fastballs.csv")

