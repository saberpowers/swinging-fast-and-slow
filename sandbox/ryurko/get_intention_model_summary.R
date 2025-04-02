# PURPOSE: Set-up the approach input for Scott's downstream analysis, since the
#          rest of the model structure will be fairly massive

library(tidyverse)
library(brms)
library(tidybayes)
devtools::load_all("package/swingfastslow")

# Load the data -----------------------------------------------------------

# This is the full swing dataset, while the intent data was trained on 
# squared up contact against pitchers' primary fastballs
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


# Load the models ---------------------------------------------------------

intent_swing_length_brms <- 
  read_rds("sandbox/ryurko/models/intent_swing_length_full.rds")
intent_bat_speed_brms <- 
  read_rds("sandbox/ryurko/models/intent_bat_speed_full.rds")

# Create the approach table -----------------------------------------------

# Get the posterior means for the random effects in each model:
bat_speed_re <- intent_bat_speed_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  # already grouped by batter_side_id and term:
  summarize(value = mean(r_batter_side_id),
            .groups = "drop") |>
  # Make a wide dataset with a column for each:
  pivot_wider(names_from = term,
              names_glue = "{term}_bat_speed",
              values_from = value)

# Repeat for swing length
swing_length_re <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  summarize(value = mean(r_batter_side_id),
            .groups = "drop") |>
  pivot_wider(names_from = term,
              names_glue = "{term}_swing_length",
              values_from = value)

# Create and save the approach table:
approach <- bat_speed_re |>
  dplyr::select(batter_side_id, strikes_bat_speed) |>
  inner_join(dplyr::select(swing_length_re,
                           batter_side_id, strikes_swing_length),
             by = "batter_side_id")
write_csv(approach, "sandbox/ryurko/models/results/approach.csv")

# Will get the other necessary pieces for results later...