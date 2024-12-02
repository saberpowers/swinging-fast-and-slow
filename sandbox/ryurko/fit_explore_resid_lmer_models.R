# PURPOSE: Fit lmer models to residuals to break down timing 

library(tidyverse)
devtools::load_all("package/swingfastslow")

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

full_swing_data <- full_swing_data |>
  mutate(row_index = 1:n())

# Load and join intention parameters --------------------------------------

intended_bat_speed_summary <- 
  read_rds("sandbox/ryurko/models/intent_bat_speed_params_all_swings.rds")

intended_swing_length_summary <- 
  read_rds("sandbox/ryurko/models/intent_swing_length_params_all_swings.rds")

# It's just in order, bind cols of the predictions
full_swing_data <- full_swing_data |>
  bind_cols(dplyr::select(intended_bat_speed_summary, -row_id),
            dplyr::select(intended_swing_length_summary, -row_id)) |>
  # Get the intended swing predictions
  mutate(resid_bat_speed = bat_speed - mode_speed,
         resid_swing_length = swing_length - mode_length,
         # Get on % scale residuals
         percent_resid_bat_speed = resid_bat_speed / mode_speed,
         percent_resid_swing_length = resid_swing_length / mode_length)


# Model residual % based on pitch characteristics -------------------------

library(lme4)
full_swing_data <- full_swing_data |>
  # Adjust horizontal break based on handedness
  mutate(horz_break_ref = ifelse(bat_side == "R", horz_break, -horz_break))

full_swing_data |>
  ggplot(aes(x = horz_break)) +
  geom_histogram() +
  facet_wrap(~ pitch_type) +
  theme_light()

full_swing_data |>
  filter(pitch_type == "CU") |>
  ggplot(aes(x = horz_break_ref)) +
  geom_histogram() +
  facet_wrap(~ bat_side) +
  theme_light()
# Positive means break away from batter...


# Only using pitches without missing characteristics:
model_swing_data <- full_swing_data |>
  filter(!is.na(release_speed), !is.na(horz_break_ref), !is.na(induced_vert_break)) |>
  # Make standardized versions of variables:
  mutate(horz_break_std = (horz_break_ref - mean(horz_break_ref, na.rm = TRUE)) / sd(horz_break_ref),
         induced_vert_break_std = (induced_vert_break - mean(induced_vert_break)) / 
           sd(induced_vert_break),
         release_speed_std = (release_speed - mean(release_speed)) / sd(release_speed))


# First for bat speed:
bat_speed_resid_fit <- lmer(
  percent_resid_bat_speed ~ horz_break_ref + induced_vert_break + release_speed + 
    (0 + release_speed | batter_side_id),
  data = full_swing_data
)
# Run into convergence issues... need to standardize variables

summary(bat_speed_resid_fit)

# Fit the standardized version of variables
bat_speed_std_resid_fit <- lmer(
  percent_resid_bat_speed ~ horz_break_std + induced_vert_break_std + release_speed_std + 
    (0 + release_speed_std | batter_side_id),
  data = model_swing_data
)

summary(bat_speed_std_resid_fit)

# What happens if only release speed is in the model?
bat_speed_release_speed_resid_fit <- lmer(
  percent_resid_bat_speed ~ release_speed_std + 
    (0 + release_speed_std | batter_side_id),
  #control = lme4::lmerControl(optimizer = "bobyqa"),
  data = model_swing_data
)

summary(bat_speed_release_speed_resid_fit)
# Okay it's same direction... will stick with break characteristics


# Repeat for swing length:
swing_length_std_resid_fit <- lmer(
  percent_resid_swing_length ~ horz_break_std + induced_vert_break_std + release_speed_std + 
    (0 + release_speed_std | batter_side_id),
  data = model_swing_data
)

summary(swing_length_std_resid_fit)

# Explore the random effects ----------------------------------------------

speed_slopes <- broom.mixed::tidy(bat_speed_std_resid_fit, effects = "ran_vals")
length_slopes <- broom.mixed::tidy(swing_length_std_resid_fit, effects = "ran_vals")

batter_table <- model_swing_data |>
  filter(!is.na(percent_resid_swing_length), !is.na(percent_resid_bat_speed)) |>
  group_by(batter_side_id, batter_id, batter_name) |>
  summarize(n_swings = n(),
            n_squared_up = sum(as.numeric(squared_up), na.rm = TRUE),
            sd_perc_resid_speed = sd(percent_resid_bat_speed),
            sd_perc_resid_length = sd(percent_resid_swing_length),
            .groups = "drop") |>
  mutate(squared_up_rate = n_squared_up / n_swings)

# Join the random slopes to the table:
batter_table <- batter_table |>
  left_join(dplyr::select(rename(speed_slopes, speed_slope = estimate),
                          level, speed_slope),
            by = c("batter_side_id" = "level")) |>
  left_join(dplyr::select(rename(length_slopes, length_slope = estimate),
                          level, length_slope),
            by = c("batter_side_id" = "level"))

# Distribution of n_squared_up?
batter_table |>
  ggplot(aes(x = n_squared_up)) +
  stat_ecdf() +
  theme_light()
# Eh use 25 swings...

batter_table |>
  filter(n_squared_up >= 25) |>
  ggplot(aes(x = speed_slope, y = length_slope)) +
  geom_smooth() +
  geom_text(aes(label = batter_name), size = 1.5) +
  labs(x = "Random slope for bat speed residual%",
       y = "Random slope for swing length residual%",
       title = "Random slopes with release speed for residual% models",
       subtitle = "Displaying batters with at least 25 squared-up swings") +
  theme_light()


# Display variances

batter_table |>
  filter(n_squared_up >= 25) |>
  ggplot(aes(x = sd_perc_resid_speed, y = sd_perc_resid_length)) +
  geom_smooth() +
  geom_text(aes(label = batter_name), size = 1.5) +
  labs(x = "Standard deviation of bat speed residual%",
       y = "Standard deviation of swing length residual%",
       title = "Standard deviations of residul%",
       subtitle = "Displaying batters with at least 25 squared-up swings") +
  theme_light()



