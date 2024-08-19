# PURPOSE: Explore modeling intended length and speed using only fastballs,
#          then explore how % above/below intended with all swings relates to

library(tidyverse)
devtools::load_all("package/swingfastslow")
library(lme4)

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


# Fit intention models using only 4-seam fastballs ------------------------

# First bat-speed
intent_bat_speed_fit <- lmer(
  bat_speed ~ balls + strikes + plate_x_ref + plate_z +
    (1 + strikes + plate_x_ref + plate_z | batter_side_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data |>
    dplyr::filter(squared_up, pitch_type == "FF")
)
# Take a look at the summary...
summary(intent_bat_speed_fit)

# Partition R^2
rsq::rsq.lmm(intent_bat_speed_fit, adj = TRUE)
# $model
# [1] 0.4604271
# 
# $fixed
# [1] 0.09563819
# 
# $random
# [1] 0.3647889

# Next swing length:
intent_swing_length_fit <- lmer(
  swing_length ~ balls + strikes + plate_x_ref + plate_z +
    (1 + strikes + plate_x_ref + plate_z | batter_side_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data |>
    dplyr::filter(squared_up, pitch_type == "FF")
)
summary(intent_swing_length_fit)

# Partition R^2
rsq::rsq.lmm(intent_swing_length_fit, adj = TRUE)
# $model
# [1] 0.6044829
# 
# $fixed
# [1] 0.181113
# 
# $random
# [1] 0.4233699


# Generate the intended and residuals from models -------------------------

full_swing_data <- full_swing_data |>
  # Get the intended swing predictions
  mutate(intent_bat_speed = predict(intent_bat_speed_fit,
                                    newdata = full_swing_data,
                                    allow.new.levels = TRUE),
         intent_swing_length = predict(intent_swing_length_fit,
                                       newdata = full_swing_data,
                                       allow.new.levels = TRUE),
         # Compute the residuals
         resid_bat_speed = bat_speed - intent_bat_speed,
         resid_swing_length = swing_length - intent_swing_length,
         # Get on % scale residuals
         percent_resid_bat_speed = resid_bat_speed / intent_bat_speed,
         percent_resid_swing_length = resid_swing_length / intent_swing_length)


# Examine basic summaries of player deviations ----------------------------

# View marginal distributions at first:
full_swing_data |>
  ggplot(aes(x = percent_resid_bat_speed)) +
  geom_histogram() +
  geom_vline(xintercept = mean(full_swing_data$percent_resid_bat_speed,
                               na.rm = TRUE),
             linetype = "dashed", color = "red") +
  labs(x = "Bat speed residual %") +
  theme_bw()
# slight left skew... but mode at 0, mean slightly negative

full_swing_data |>
  ggplot(aes(x = percent_resid_swing_length)) +
  geom_histogram() +
  geom_vline(xintercept = mean(full_swing_data$percent_resid_swing_length,
                               na.rm = TRUE),
             linetype = "dashed", color = "red") +
  labs(x = "Swing length residual %") +
  theme_bw()
# that's pretty symmetric - but does not look centered at 0, almost appears 
# shifted slightly above - that may make sense though since the intention model
# is based on squared up contact on fastballs, while swing and miss will likely
# result in longer swings

# Joint distribution:
full_swing_data |>
  ggplot(aes(x = percent_resid_swing_length, y = percent_resid_bat_speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "Swing length residual %", y = "Bat speed residual %") +
  theme_bw()
# Nice - this is consistent with what we saw before in the abstract, so that's
# good to see again

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
  labs(x = "Std dev of swing length residual%",
       y = "Std dev of bat speed residual%",
       title = "Player-level summary of swing-level variation from intention",
       subtitle = "Only displaying players with minimum of 30 squared-up swings") +
  coord_fixed() +
  theme_bw()


# Model deviations following Noah's approach ------------------------------

# TO ADD: missing the pitcher IDs!

# Just going to start simple with this, no batter random effects (should that
# be in here???) but will match Noah's model but with additive linear terms

# First, swing length residual:
swing_length_resid_fit <- lmer(
  percent_resid_swing_length ~ balls + strikes + plate_x_ref + plate_z + 
    horz_break + induced_vert_break + release_speed + (1 | pitcher_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data
)
summary(swing_length_resid_fit)
rsq::rsq.lmm(swing_length_resid_fit, adj = TRUE)
# $model
# [1] 0.4278684
# 
# $fixed
# [1] 0.2105308
# 
# $random
# [1] 0.2173376


# First bat-speed
bat_speed_resid_fit <- lmer(
  percent_resid_bat_speed ~ percent_resid_swing_length + 
    balls + strikes + plate_x_ref + plate_z + 
    horz_break + induced_vert_break + release_speed + (1 | pitcher_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data
)
# Take a look at the summary...
summary(bat_speed_resid_fit)

rsq::rsq.lmm(bat_speed_resid_fit, adj = TRUE)
# $model
# [1] 0.2525762
# 
# $fixed
# [1] 0.2075536
# 
# $random
# [1] 0.04502255
# Interesting difference in bat speed model versus swing length, way more
# of the variation is explained by pitcher random effects for length than speed 

# What about removing swing length deviation - what impact does that have?
bat_speed_resid_fit_wo_length <- lmer(
  percent_resid_bat_speed ~ balls + strikes + plate_x_ref + plate_z + 
    horz_break + induced_vert_break + release_speed + (1 | pitcher_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = full_swing_data
)
# Take a look at the summary...
summary(bat_speed_resid_fit_wo_length)
rsq::rsq.lmm(bat_speed_resid_fit_wo_length, adj = TRUE)
# $model
# [1] 0.05668959
# 
# $fixed
# [1] 0.04565023
# 
# $random
# [1] 0.01103936
# WOW

# Should probably model this with GAM as well... but does it make sense to also
# include batter random effects in this model, how their deviations from intention
# might rise or fall depending contextual factors?

