# PURPOSE: Compare ELPD between skew normal and normal for swing length

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

# Split into train and test datasets --------------------------------------

# Do a random 80/20 split:
set.seed(1979)
intent_swing_data <- intent_swing_data |>
  mutate(fold = sample(c("train", "test"), n(), replace = TRUE, 
                       prob = c(.8, .2)))
train_data <- intent_swing_data |>
  filter(fold == "train")
test_data <- intent_swing_data |>
  filter(fold == "test")

# Fit skew normal model ---------------------------------------------------

skew_normal_fit <- 
  brm(bf(swing_length ~ balls + strikes + plate_x_ref + plate_z + (1 | pitcher_id) +
           (1 + strikes + plate_x_ref + plate_z | batter_side_id),
         sigma ~ 1,
         alpha ~ 1 + (1 | batter_side_id)),
      family = skew_normal(),
      data = train_data,
      chains = 4,
      cores = 4,
      # Bump of samples for swing length
      iter = 5000)
write_rds(skew_normal_fit, "sandbox/ryurko/models/skew_normal_length_train_split.rds",
          compress = "gz")
pp_check(skew_normal_fit, newdata = test_data, allow_new_levels = TRUE)

# Fit Gaussian model ------------------------------------------------------

normal_fit <- 
  brm(bf(swing_length ~ balls + strikes + plate_x_ref + plate_z + (1 | pitcher_id) +
           (1 + strikes + plate_x_ref + plate_z | batter_side_id)),
      data = train_data,
      chains = 4,
      cores = 4,
      iter = 5000)
write_rds(normal_fit, "sandbox/ryurko/models/normal_length_train_split.rds",
          compress = "gz")

pp_check(normal_fit, newdata = test_data, allow_new_levels = TRUE)

# Compute ELPD with test data ---------------------------------------------

llt_skew_normal <- colMeans(log_lik(skew_normal_fit, newdata = test_data,
                                    allow_new_levels = TRUE))
llt_normal <- colMeans(log_lik(normal_fit, newdata = test_data,
                               allow_new_levels = TRUE))
llt_diff <- llt_skew_normal - llt_normal

# Compute the standard error of the sum
llt_se <- sd(llt_diff) * sqrt(length(llt_diff))
llt_se
# For 80/20 split:
#[1] 11.72371


(c(sum(llt_diff) - 2 * llt_se, sum(llt_diff), sum(llt_diff) + 2 * llt_se))
# For 80/20 split:
# [1]  3[1] 10.68047 34.12788 57.57529
# Nice! the skewed normal wins agains

# Predictive error? -------------------------------------------------------

errors_skew_normal <- 
  predictive_error(skew_normal_fit, method = "posterior_epred", 
                   newdata = test_data, allow_new_levels = TRUE)
rmse_skew_normal <- sqrt(colMeans(errors_skew_normal^2))

errors_normal <- 
  predictive_error(normal_fit, method = "posterior_epred", 
                   newdata = test_data, allow_new_levels = TRUE)
rmse_normal <- sqrt(colMeans(errors_normal^2))

rmse_diff <- rmse_skew_normal - rmse_normal
mean(rmse_diff)
# For 80/20 split:
# [1] -0.0004707677

sd(rmse_diff) / sqrt(length(rmse_diff))
# For 80/20 split:
# [1] 0.0002205563

hist(rmse_diff)

# What about allowing for all model uncertainties?
errors_skew_normal_full <- 
  predictive_error(skew_normal_fit,
                   newdata = test_data, allow_new_levels = TRUE)
rmse_skew_normal_full <- sqrt(colMeans(errors_skew_normal_full^2))

errors_normal_full <- 
  predictive_error(normal_fit,
                   newdata = test_data, allow_new_levels = TRUE)
rmse_normal_full <- sqrt(colMeans(errors_normal_full^2))

rmse_diff_full <- rmse_skew_normal_full - rmse_normal_full
mean(rmse_diff_full)
# For 80/20 split:
# [1] 7.873073e-05
# Nice eh roughly the same

sd(rmse_diff_full) / sqrt(length(rmse_diff_full))
# For 80/20 split:
# [1] 0.0001627736

hist(rmse_diff_full)
