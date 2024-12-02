# PURPOSE: Fit skewed-normal swing length intention model

library(tidyverse)
devtools::load_all("package/swingfastslow")
library(brms)
library(tidybayes)

# Load and prep data ------------------------------------------------------

full_swing_data <- read_rds("data/bat_tracking_2024_season.rds") |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
  remove_partial_swings() |>
  recreate_squared_up() |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    plate_x_ref = ifelse(bat_side == "R", plate_x, -plate_x),
    is_contact = description %in% c("foul", "hit_into_play"),
    is_fair = description == "hit_into_play")

# Create the intent swing dataset -----------------------------------------

# Load the primary fastballs table
primary_fastballs <- read_csv("data/primary_fastballs.csv")

intent_swing_data <- full_swing_data |>
  filter(pitch_type %in% c("FF", "SI", "FC")) |>
  left_join(primary_fastballs, by = "pitcher_id") |>
  mutate(is_primary = pitch_type == primary_fastball) |>
  # only use primary fastballs and squared up swings
  filter(is_primary, squared_up)

rm(full_swing_data)

# Fit the intent model ----------------------------------------------------

intent_swing_length_brms <- 
  brm(bf(swing_length ~ balls + strikes + plate_x_ref + plate_z + (1 | pitcher_id) +
           (1 + strikes + plate_x_ref + plate_z | batter_side_id),
         sigma ~ 1,
         alpha ~ 1 + (1 | batter_side_id)),
      family = skew_normal(),
      data = intent_swing_data,
      chains = 4,
      cores = 4,
      iter = 6000)

write_rds(intent_swing_length_brms, "sandbox/ryurko/models/intent_swing_length_full.rds")

# No divergences!

