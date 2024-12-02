# PURPOSE: Explore output of intention models by summarizing posterior samples

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


# Load models -------------------------------------------------------------

intent_bat_speed_brms <- read_rds("sandbox/ryurko/models/intent_bat_speed_full.rds")
intent_swing_length_brms <- read_rds("sandbox/ryurko/models/intent_swing_length_full.rds")

# Summarize the fixed effects ---------------------------------------------

# Use the b versions of the fixed effects, first for bat speed:
intent_bat_speed_brms |>
  gather_draws(b_balls, b_strikes, b_plate_x_ref, b_plate_z) |>
  mutate(.variable = str_remove(.variable, "b_")) |>
  ggplot(aes(x = .value)) +
  geom_density() +
  labs(x = "Posterior sample value") +
  facet_wrap(~.variable, ncol = 2, scales = "free_x") +
  theme_bw() 

# Next for swing length:
intent_swing_length_brms |>
  gather_draws(b_balls, b_strikes, b_plate_x_ref, b_plate_z) |>
  mutate(.variable = str_remove(.variable, "b_")) |>
  ggplot(aes(x = .value)) +
  geom_density() +
  labs(x = "Posterior sample value") +
  facet_wrap(~.variable, ncol = 2, scales = "free_x") +
  theme_bw() 

# Summarize the random effects --------------------------------------------

# Get a table of the batters
batter_table <- full_swing_data |>
  group_by(batter_side_id, batter_id, batter_name) |>
  summarize(n_swings = n(),
            n_squared_up = sum(as.numeric(squared_up), na.rm = TRUE),
            .groups = "drop") |>
  mutate(squared_up_rate = n_squared_up / n_swings)

## Bat speed model - mean effects
bat_speed_intent_summary <- intent_bat_speed_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  summarize(speed_mean = mean(r_batter_side_id), 
            speed_lower_80 = quantile(r_batter_side_id, 0.1),
            speed_upper_80 = quantile(r_batter_side_id, 0.9),
            .groups = "drop")

### Swing length model - mean effects
swing_length_intent_summary <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  summarize(length_mean = mean(r_batter_side_id), 
            length_lower_80 = quantile(r_batter_side_id, 0.1),
            length_upper_80 = quantile(r_batter_side_id, 0.9),
            .groups = "drop")

# Combine to create visualizations:
mean_intent_summary <- bat_speed_intent_summary |>
  inner_join(swing_length_intent_summary, 
             by = c("batter_side_id", "term")) |>
  left_join(batter_table, by = "batter_side_id")

# Visuals for each term (could facet, that would be intense though)
mean_intent_summary |>
  filter(n_squared_up >= 25) |>
  ggplot(aes(x = speed_mean, y = length_mean)) +
  geom_smooth(se = FALSE) +
  geom_text(aes(label = batter_name), size = 1) +
  facet_wrap(~term, ncol = 2, scales = "free") +
  labs(x = "Posterior mean for bat speed intent random effect",
       y = "Posterior mean for swing length intent random effect",
       title = "Posterior means for batter random effects (mean models)",
       subtitle = "Displaying batters with at least 25 squared-up swings") +
  theme_bw()

## Repeat for alpha terms

## Bat speed model - alpha effects
bat_speed_alpha_intent_summary <- intent_bat_speed_brms |>
  spread_draws(r_batter_side_id__alpha[batter_side_id, term]) |>
  summarize(speed_mean = mean(r_batter_side_id__alpha), 
            speed_lower_80 = quantile(r_batter_side_id__alpha, 0.1),
            speed_upper_80 = quantile(r_batter_side_id__alpha, 0.9),
            .groups = "drop")

### Swing length model - mean effects
swing_length_alpha_intent_summary <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id__alpha[batter_side_id, term]) |>
  summarize(length_mean = mean(r_batter_side_id__alpha), 
            length_lower_80 = quantile(r_batter_side_id__alpha, 0.1),
            length_upper_80 = quantile(r_batter_side_id__alpha, 0.9),
            .groups = "drop")

# Combine to create visualizations:
alpha_intent_summary <- bat_speed_alpha_intent_summary |>
  inner_join(swing_length_alpha_intent_summary, 
             by = c("batter_side_id", "term")) |>
  left_join(batter_table, by = "batter_side_id")

# Visuals for each term (could facet, that would be intense though)
alpha_intent_summary |>
  filter(n_squared_up >= 25) |>
  ggplot(aes(x = speed_mean, y = length_mean)) +
  geom_smooth(se = FALSE) +
  geom_text(aes(label = batter_name), size = 1.5) +
  labs(x = "Posterior mean for bat speed alpha random effect",
       y = "Posterior mean for swing length alpha random effect",
       title = "Posterior means for batter random effects (alpha models)",
       subtitle = "Displaying batters with at least 25 squared-up swings") +
  theme_bw()

