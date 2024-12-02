# PURPOSE: Generate the intended parameters for every pitch in the dataset

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

# Create a batch index that will be used to generate the predictions with
BATCH_SIZE <- 300 # value that can be changed 
full_swing_data <- full_swing_data |>
  mutate(row_index = 1:n(),
         row_batch = rep(1:BATCH_SIZE, length.out = n()))

# Generate the bat speed parameters ---------------------------------------

N_DRAWS <- 2000 # subset of posterior to faster computation

intent_bat_speed_brms <- read_rds("sandbox/ryurko/models/intent_bat_speed_full.rds")

intended_bat_speed_summary <- 
  map_dfr(unique(full_swing_data$row_batch),
          function(batch_i) {
            
            add_epred_draws(intent_bat_speed_brms,
                            newdata = full_swing_data |>
                              dplyr::filter(row_batch == batch_i),
                            allow_new_levels = TRUE,
                            ndraws = N_DRAWS, 
                            dpar = c("mu", "sigma", "alpha")) |>
              ungroup() |>
              dplyr::select(.row, mu, sigma, alpha) |>
              rename(row_id = .row) |>
              mutate(delta = alpha / (sqrt(1 + alpha^2)),
                     scale = sigma / (sqrt(1 - (2 / pi) * (delta^2))),
                     location = mu - scale * delta * sqrt(2 / pi),
                     m0_alpha = sqrt(2 / pi) * delta - (1 - pi / 4) * 
                       ((sqrt(2 / pi) * delta)^3) / (1 - (2 / pi) * delta^2) - 
                       (sign(alpha) / 2) * exp(-2 * pi / abs(alpha)),
                     mode = location + scale * m0_alpha) |>
              group_by(row_id) |>
              summarize(mean_speed = mean(mu, na.rm = TRUE),
                        sigma_speed = mean(sigma, na.rm = TRUE),
                        alpha_speed = mean(alpha, na.rm = TRUE),
                        mode_speed = mean(mode, na.rm = TRUE),
                        .groups = "drop")
          })

# Save:
write_rds(intended_bat_speed_summary, 
          "sandbox/ryurko/models/intent_bat_speed_params_all_swings.rds")

rm(intent_bat_speed_brms)

# Generate the swing length parameters ------------------------------------

intent_swing_length_brms <- read_rds("sandbox/ryurko/models/intent_swing_length_full.rds")

intended_swing_length_summary <- 
  map_dfr(unique(full_swing_data$row_batch),
          function(batch_i) {
            
            add_epred_draws(intent_swing_length_brms,
                            newdata = full_swing_data |>
                              dplyr::filter(row_batch == batch_i),
                            ndraws = N_DRAWS, 
                            allow_new_levels = TRUE,
                            dpar = c("mu", "sigma", "alpha")) |>
              ungroup() |>
              dplyr::select(.row, mu, sigma, alpha) |>
              rename(row_id = .row) |>
              mutate(delta = alpha / (sqrt(1 + alpha^2)),
                     scale = sigma / (sqrt(1 - (2 / pi) * (delta^2))),
                     location = mu - scale * delta * sqrt(2 / pi),
                     m0_alpha = sqrt(2 / pi) * delta - (1 - pi / 4) * 
                       ((sqrt(2 / pi) * delta)^3) / (1 - (2 / pi) * delta^2) - 
                       (sign(alpha) / 2) * exp(-2 * pi / abs(alpha)),
                     mode = location + scale * m0_alpha) |>
              group_by(row_id) |>
              summarize(mean_length = mean(mu, na.rm = TRUE),
                        sigma_length = mean(sigma, na.rm = TRUE),
                        alpha_length = mean(alpha, na.rm = TRUE),
                        mode_length = mean(mode, na.rm = TRUE),
                        .groups = "drop")
          })

# Save:
write_rds(intended_swing_length_summary,
          "sandbox/ryurko/models/intent_swing_length_params_all_swings.rds")



