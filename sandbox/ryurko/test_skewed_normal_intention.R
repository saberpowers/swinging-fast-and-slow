# PURPOSE: Test skewed-normal modeling for intention models using brms
#          (TO DO - REVISE THE FASTBALLS TO BE PRIMARY FASTBALLS!)

library(tidyverse)
devtools::load_all("package/swingfastslow")
library(brms)
library(tidybayes)

# Load and prep data ------------------------------------------------------

full_swing_data <- read_rds("data/bat_tracking_0817.rds") |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
  remove_partial_swings() |>
  recreate_squared_up() |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    plate_x_ref = ifelse(bat_side == "R", plate_x, -plate_x),
    is_contact = description %in% c("foul", "hit_into_play"),
    is_fair = description == "hit_into_play")

# Fit bat speed intention model -------------------------------------------

# First bat-speed (just starting w/ simplest version to see if it works...)
intent_bat_speed_brms <- 
  brm(bf(bat_speed ~ balls + strikes + plate_x_ref + plate_z +
        (1 + strikes + plate_x_ref + plate_z | batter_side_id),
      sigma ~ 1,
      alpha ~ 1 + (1 | batter_side_id)),
      family = skew_normal(),
      data = full_swing_data |>
        dplyr::filter(squared_up, pitch_type == "FF"),
      chains = 4,
      cores = 4,
      iter = 4000)
# NO DIVERGENCES! and no warnings as well - so we're in a good setting then...

# Save this model:
write_rds(intent_bat_speed_brms, "sandbox/ryurko/models/intent_bat_speed_brms.rds")
intent_bat_speed_brms <- read_rds("sandbox/ryurko/models/intent_bat_speed_brms.rds")

# Take a look at the summary...
summary(intent_bat_speed_brms)
# the variances of the random effects look similar to before (which is good) and
# there appears to be moderately high variance for the alpha intercepts, meaning
# there is variation in the shape of the intention distributions...

# THIS WORKS! This makes my life so much easier....
test_preds <- add_epred_draws(intent_bat_speed_brms,
                              newdata = full_swing_data |>
                                dplyr::filter(squared_up, pitch_type == "FF") |>
                                slice(1:10),
                              dpar = c("mu", "sigma", "alpha")) |>
  mutate(delta = alpha / (sqrt(1 + alpha^2)),
         scale = sigma / (sqrt(1 - (2 / pi) * (delta^2))),
         location = mu - scale * delta * sqrt(2 / pi),
         m0_alpha = sqrt(2 / pi) * delta - (1 - pi / 4) * 
           ((sqrt(2 / pi) * delta)^3) / (1 - (2 / pi) * delta^2) - 
           (sign(alpha) / 2) * exp(-2 * pi / abs(alpha)),
         mode = location + scale * m0_alpha)



# Generate the intended bat speed modes and compute residuals -------------

# Since this relies on the posterior samples, it order to make it reasonably
# efficient, I'll need to loop over batches of observations in some way...

full_swing_data <- full_swing_data |>
  mutate(row_index = 1:n(),
         row_batch = rep(1:300, length.out = n()))

intended_bat_speed_summary <- 
  map_dfr(unique(full_swing_data$row_batch),
          function(batch_i) {
            
            add_epred_draws(intent_bat_speed_brms,
                            newdata = full_swing_data |>
                              dplyr::filter(row_batch == batch_i),
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
              summarize(mean_speed = mean(mu, na.rm = TRUE),
                        sigma_speed = mean(sigma, na.rm = TRUE),
                        alpha_speed = mean(alpha, na.rm = TRUE),
                        mode_speed = mean(mode, na.rm = TRUE),
                        .groups = "drop")
          })
# Save this for access:
write_rds(intended_bat_speed_summary, "sandbox/ryurko/models/intended_bat_speed_summary.rds")


# Nice - looks good!

# Fit swing speed intention model -----------------------------------------

intent_swing_length_brms <- 
  brm(bf(swing_length ~ balls + strikes + plate_x_ref + plate_z +
           (1 + strikes + plate_x_ref + plate_z | batter_side_id),
         sigma ~ 1,
         alpha ~ 1 + (1 | batter_side_id)),
      family = skew_normal(),
      data = full_swing_data |>
        dplyr::filter(squared_up, pitch_type == "FF"),
      chains = 4,
      cores = 4,
      iter = 6000)
# No more warnings...
# Save this model:
write_rds(intent_swing_length_brms, "sandbox/ryurko/models/intent_swing_length_brms.rds")
intent_swing_length_brms <- read_rds("sandbox/ryurko/models/intent_swing_length_brms.rds")


# Get the intention summary:
intended_swing_length_summary <- 
  map_dfr(unique(full_swing_data$row_batch),
          function(batch_i) {
            
            add_epred_draws(intent_swing_length_brms,
                            newdata = full_swing_data |>
                              dplyr::filter(row_batch == batch_i),
                            ndraws = 1000, # just for ease use subset
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
# Save this for access:
write_rds(intended_swing_length_summary, "sandbox/ryurko/models/intended_swing_length_summary.rds")


# Perform the next stage model --------------------------------------------

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


# View marginal distributions at first:
full_swing_data |>
  ggplot(aes(x = resid_bat_speed)) +
  geom_histogram() +
  geom_vline(xintercept = mean(full_swing_data$resid_bat_speed,
                               na.rm = TRUE),
             linetype = "dashed", color = "red") +
  labs(x = "Bat speed residual") +
  theme_bw()

full_swing_data |>
  ggplot(aes(x = percent_resid_bat_speed)) +
  geom_histogram() +
  geom_vline(xintercept = mean(full_swing_data$percent_resid_bat_speed,
                               na.rm = TRUE),
             linetype = "dashed", color = "red") +
  labs(x = "Bat speed residual %") +
  theme_bw()
# shifted slightly negative

full_swing_data |>
  ggplot(aes(x = percent_resid_swing_length)) +
  geom_histogram() +
  geom_vline(xintercept = mean(full_swing_data$percent_resid_swing_length,
                               na.rm = TRUE),
             linetype = "dashed", color = "red") +
  labs(x = "Swing length residual %") +
  theme_bw()

# Joint distribution:
full_swing_data |>
  ggplot(aes(x = percent_resid_swing_length, y = percent_resid_bat_speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "Swing length residual %", y = "Bat speed residual %") +
  theme_bw()
# Huh slightly different from before

full_swing_data |>
  ggplot(aes(x = mode_length, y = mode_speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "Intended swing length", y = "Intended bat speed") +
  theme_bw()

full_swing_data |>
  mutate(is_stanton = str_detect(batter_name, "Giancarlo")) |>
  ggplot(aes(x = mode_length, y = mode_speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(~ is_stanton) +
  labs(x = "Intended swing length", y = "Intended bat speed") +
  theme_bw()

full_swing_data |>
  ggplot(aes(x = resid_swing_length, y = resid_bat_speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "Swing length residual", y = "Bat speed residua") +
  theme_bw()

# Look at summarizing the residual%s at the player level:
batter_resid_summary <- full_swing_data |>
  filter(!is.na(percent_resid_swing_length), !is.na(percent_resid_bat_speed)) |>
  group_by(batter_id, batter_name) |>
  summarize(n_swings = n(),
            n_squared_up = sum(as.numeric(squared_up), na.rm = TRUE),
            sd_perc_resid_speed = sd(percent_resid_bat_speed),
            sd_perc_resid_length = sd(percent_resid_swing_length),
            .groups = "drop")

# Raw plot of everyone:
batter_resid_summary |>
  ggplot(aes(x = sd_perc_resid_length, y = sd_perc_resid_speed)) +
  geom_point(alpha = 0.5) +
  theme_bw()
# Too many with few swings

# ECDF of n_squared_up - since that determined intention model (although so did FFs...)
batter_resid_summary |>
  ggplot(aes(x = n_squared_up)) +
  stat_ecdf() +
  # Scott's old cutoff:
  geom_vline(xintercept = 30, linetype = "dashed", color = "red") +
  theme_bw()
# eh not bad for this purpose


batter_resid_summary |>
  filter(n_squared_up >= 30) |>
  ggplot(aes(x = sd_perc_resid_length, y = sd_perc_resid_speed)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(x = "Std dev of swing length residual%",
       y = "Std dev of bat speed residual%",
       title = "Player-level summary of swing-level variation from intention",
       subtitle = "Only displaying players with minimum of 30 squared-up swings") +
  coord_fixed() +
  theme_bw()

batter_resid_summary |>
  filter(n_squared_up >= 30) |>
  ggplot(aes(x = sd_perc_resid_length, y = sd_perc_resid_speed)) +
  geom_text(aes(label = batter_name), size = 1.5) +
  labs(x = "Std dev of swing length residual%",
       y = "Std dev of bat speed residual%",
       title = "Player-level summary of swing-level variation from intention",
       subtitle = "Only displaying players with minimum of 30 squared-up swings") +
  coord_fixed() +
  theme_bw()


# Model swing length deviations as function of speed ----------------------

library(lme4)
swing_length_speed_fit <- lmer(
  percent_resid_swing_length ~  release_speed + (0 + release_speed | batter_side_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data
)

# Take a look at the summary...
summary(swing_length_speed_fit)
rsq::rsq.lmm(swing_length_speed_fit, adj = TRUE)

batter_table <- full_swing_data |>
  dplyr::select(batter_side_id, batter_id, batter_name) |>
  distinct()

# Extract the random effects:
speed_slopes <- broom.mixed::tidy(swing_length_speed_fit, effects = "ran_vals")

batter_summary_table <- speed_slopes |>
  inner_join(batter_table, by = c("level" = "batter_side_id")) 

# Get the intended swing length posterior means and shapes:
swing_length_intended_ints <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  filter(term == "Intercept") |>
  summarize(posterior_mean = mean(r_batter_side_id), 
            posterior_median = median(r_batter_side_id),
            lower_80 = quantile(r_batter_side_id, 0.1),
            upper_80 = quantile(r_batter_side_id, 0.9),
            .groups = "drop")

swing_length_intended_alpha_ints <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id__alpha[batter_side_id, term]) |>
  filter(term == "Intercept") |>
  summarize(posterior_alpha_mean = mean(r_batter_side_id__alpha), 
            posterior_alpha_median = median(r_batter_side_id__alpha),
            lower_alpha_80 = quantile(r_batter_side_id__alpha, 0.1),
            upper_alpha_80 = quantile(r_batter_side_id__alpha, 0.9),
            .groups = "drop")

swing_length_summary <- swing_length_intended_ints |>
  dplyr::select(batter_side_id, posterior_mean) |>
  inner_join(dplyr::select(swing_length_intended_alpha_ints, 
                           batter_side_id, posterior_alpha_mean),
             by = "batter_side_id")

swing_length_summary |>
  inner_join(batter_table, by = "batter_side_id") |>
  ggplot(aes(x = posterior_mean, y = posterior_alpha_mean)) +
  geom_text(aes(label = batter_name),
            size = 2, alpha = 0.75) +
  theme_minimal()
# Huh kind of interesting....

# See how the residual random effect compares:
batter_summary_table |>
  inner_join(swing_length_summary, 
             by = c("level" = "batter_side_id")) |>
  ggplot(aes(x = posterior_mean, y = estimate)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = batter_name),
            size = 2, alpha = 0.75) +
  theme_minimal()

# Model deviations following Noah's approach ------------------------------


library(lme4)
bat_speed_resid_fit <- lmer(
  percent_resid_bat_speed ~ percent_resid_swing_length + 
    horz_break + induced_vert_break + release_speed + (1 | pitcher_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data
)
# Take a look at the summary...
summary(bat_speed_resid_fit)
rsq::rsq.lmm(bat_speed_resid_fit, adj = TRUE)
# $model
# [1] 0.2550268
# 
# $fixed
# [1] 0.2264188
# 
# $random
# [1] 0.02860805

# Get the residuals from this model (naming convention here is horrible):
full_swing_data <- full_swing_data |>
  mutate(pred_percent_resid_bat_speed = 
           as.numeric(predict(bat_speed_resid_fit, newdata = full_swing_data,
                              allow.new.levels = TRUE)),
         resid_percent_resid_bat_speed = percent_resid_bat_speed - 
           pred_percent_resid_bat_speed)

hist(full_swing_data$resid_percent_resid_bat_speed)
# Well that just looks like a skewed normal...

full_swing_data |>
  ggplot(aes(x = pred_percent_resid_bat_speed,
             y = percent_resid_bat_speed)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "white") +
  #coord_fixed() +
  scale_fill_gradient(low = "darkblue", 
                      high = "darkorange") + 
  labs(x = "Predicted residual %", y = "Actual residual %") +
  theme_bw() +
  theme(legend.position = "bottom")

full_swing_data |>
  ggplot(aes(x = resid_percent_resid_swing_length,
             y = resid_percent_resid_bat_speed)) +
  geom_point(alpha = .1) + 
  #coord_fixed() +
  scale_fill_gradient(low = "darkblue", 
                      high = "darkorange") + 
  theme_bw() +
  theme(legend.position = "bottom")

# What about putting it back on the bat speed scale?
full_swing_data <- full_swing_data |>
  mutate(adj_bat_speed_resid = pred_percent_resid_bat_speed * mode_speed)
hist(full_swing_data$adj_bat_speed_resid)
# ehhh that's just normal-like...

# What about for swing length?
swing_length_resid_fit <- lmer(
  percent_resid_swing_length ~ horz_break + induced_vert_break + release_speed + (1 | pitcher_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data
)

full_swing_data |>
  ggplot(aes(x = release_speed)) +
  geom_histogram() +
  theme_light()

full_swing_data <- full_swing_data |>
  mutate(pred_percent_resid_swing_length = 
           as.numeric(predict(swing_length_resid_fit, newdata = full_swing_data,
                              allow.new.levels = TRUE)),
         resid_percent_resid_swing_length = percent_resid_swing_length - 
           pred_percent_resid_swing_length)

hist(full_swing_data$resid_percent_resid_swing_length)
# Well that just looks like a skewed normal...

hist(full_swing_data$pred_percent_resid_swing_length)
full_swing_data <- full_swing_data |>
  mutate(adj_swing_length_resid = resid_percent_resid_swing_length * mode_length)
hist(full_swing_data$adj_swing_length_resid)
# Ohhhh now we have something going on....

# What's this joint distribution look like?
full_swing_data |>
  ggplot(aes(x = adj_swing_length_resid, y = adj_bat_speed_resid)) +
  geom_point(alpha = 0.1) +
  theme_bw()

