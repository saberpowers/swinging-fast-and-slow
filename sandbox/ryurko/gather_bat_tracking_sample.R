# PURPOSE: Gather initial data from the season so far (8/17/24)

#          Updated to use the sabRmetrics package for all data gathering

library(tidyverse)
#source("sandbox/ryurko/download_savant_data.R")

# Gather the data ---------------------------------------------------------

START_DATE <- "2024-04-03"
END_DATE <- "2024-08-17"

current_season_data <- sabRmetrics::download_baseballsavant(START_DATE, END_DATE)

# Filter to bat tracking data and save ------------------------------------

bat_tracking_data <- current_season_data |>
  filter(!is.na(bat_speed), !is.na(swing_length)) |>
  dplyr::select( # added in pitcher_id for model
    game_date, game_id, event_index, pitcher_id, pitch_number, batter_id, bat_side, batter_name,
    inning, half_inning, outs, balls, strikes,
    ax, ay, az, vx0, vy0, vz0, release_pos_x, release_pos_y, release_pos_z,
    pitch_type, release_speed, plate_x, plate_z, strike_zone_top, strike_zone_bottom,
    bat_speed, swing_length, launch_speed, launch_angle, hit_coord_x, hit_coord_y,
    description, des)

write_rds(bat_tracking_data, "data/bat_tracking_0817.rds", compress = "gz")
