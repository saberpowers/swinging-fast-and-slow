# PURPOSE: Explore bat tracking data - just basic exploration of distributions,
#          relationships with pitch types and locations

library(tidyverse)

# Read in the bat tracking data -------------------------------------------

bat_tracking_data <- read_rds("data/bat_tracking_0521.rds") |>
  # Add some additional columns following Scott's code:
  rename(cx = release_pos_x,
         cy = release_pos_y,
         cz = release_pos_z) |>
  mutate(y0 = 50,
         t0 = -(-vy0 - sqrt(vy0^2 - 4 * (ay / 2) * (y0 - cy))) / (2 * (ay / 2)),
         bx = vx0 + (-t0) * ax,
         by = vy0 + (-t0) * ay,
         bz = vz0 + (-t0) * az,
         plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
         plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
         plate_speed = 0.6818182 * sqrt((ax * plate_time + bx)^2 +
                                          (ay * plate_time + by)^2 + (az * plate_time + bz)^2),
         squared_up = ifelse(description == "hit_into_play" & !is.na(launch_speed),
                             (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
                             FALSE),
         # Infer spray angle from hit coordinates, and reflect spray angle and plate x for LHBs
         spray_angle = (atan((hc_x - 125) / (205 - hc_y))) * 180 / pi, # radian to degree
         spray_angle_ref = ifelse(stand == "L", -spray_angle, spray_angle),
         plate_x_ref = ifelse(stand == "L", -plate_x, plate_x),
         is_bunt = str_detect(description, "bunt") | 
           (description == "hit_into_play" & str_detect(des, " bunt"))) 

# Figure out what's going on with bunts -----------------------------------

bat_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = is_bunt)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~description) +
  theme_bw()
# Well that effectively captures it!

# What's the max bunt length and bat speed?
max_bunt_values <- bat_tracking_data |>
  filter(is_bunt) |>
  summarize(length = max(swing_length, na.rm = TRUE),
            speed = max(bat_speed, na.rm = TRUE))
max_bunt_values
#length    speed
#1 3.16321 29.66294

# Add these to the previous plots:
bat_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = is_bunt)) +
  geom_vline(xintercept = max_bunt_values$length, 
             linetype = "dashed", color = "black") +
  geom_hline(yintercept = max_bunt_values$speed, 
             linetype = "dashed", color = "black") +
  geom_point(alpha = 0.25) +
  facet_wrap(~description) +
  theme_bw()

# Remove the bunts:
swing_tracking_data <- bat_tracking_data |>
  filter(!is_bunt) |>
  # Create competitive swings indicator variable
  group_by(batter) |>
  mutate(is_competitive_swing = 
           bat_speed > quantile(bat_speed, probs = 0.1, na.rm = TRUE)) |>
  ungroup() |>
  # Add an indicator if swing is in bunt zone:
  mutate(in_bunt_zone = 
           (swing_length <= max_bunt_values$length |
              bat_speed <= max_bunt_values$speed)) 

table(swing_tracking_data$in_bunt_zone)
# FALSE  TRUE 
# 83873   230 

# Remove these in_bunt_zone for now:
swing_tracking_data <- swing_tracking_data |>
  filter(!in_bunt_zone)

# Examine the competitive swing indicators --------------------------------

# Add in the competitive swings color...
swing_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = is_competitive_swing)) +
  geom_point(alpha = 0.25) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")
# Hmmm I'm not sure I like this as a cutoff

# How many of these are squared up?
swing_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = is_competitive_swing)) +
  geom_point(alpha = 0.25) +
  ggthemes::scale_color_colorblind() +
  facet_wrap(~squared_up) +
  theme_bw() +
  theme(legend.position = "bottom")
# That's a pretty substantial fraction of swings that are squared up but lost
# due to the 10% cutoff...

# What's the cutoff for every batter?
batter_comp_cutoff <- swing_tracking_data |>
  group_by(batter) |>
  summarize(cutoff = quantile(bat_speed, probs = 0.1, na.rm = TRUE),
            .groups = "drop")
batter_comp_cutoff |>
  ggplot(aes(x = cutoff)) +
  geom_histogram() +
  # Add a line for the overall 10th percentile:
  # geom_vline(xintercept = quantile(swing_tracking_data$bat_speed, 
  #                                  probs = 0.05, na.rm = TRUE),
  #            color = "red", linetype = "dashed") +
  theme_bw()
# What is this global cutoff?
quantile(swing_tracking_data$bat_speed, 
         probs = 0.05, na.rm = TRUE)
# 57.99081 

# How does this compare with the square up plot?
swing_tracking_data |>
  mutate(is_new_comp_swing = bat_speed > quantile(swing_tracking_data$bat_speed, 
                                                  probs = 0.05, na.rm = TRUE)) |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = is_new_comp_swing)) +
  geom_point(alpha = 0.25) +
  ggthemes::scale_color_colorblind() +
  facet_wrap(~squared_up) +
  theme_bw() +
  theme(legend.position = "bottom")

# Hmm not sure I like this either... I almost feel like a cutoff of 50% is not bad
# need to think about this more...

swing_tracking_data |>
  ggplot(aes(x = swing_length)) +
  geom_histogram() +
  theme_bw()
swing_tracking_data |>
  ggplot(aes(x = swing_length, color = is_competitive_swing)) +
  geom_density() +
  theme_bw()

swing_tracking_data |>
  ggplot(aes(x = bat_speed)) +
  stat_ecdf() +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
  theme_bw()
bat_speed_ecdf <- ecdf(swing_tracking_data$bat_speed)
bat_speed_ecdf(50)
# [1] 0.02587304
# Basically dropping the bottom 2.5%, not bad... I think I'll stick with this
# for now...

comp_swing_tracking_data <- swing_tracking_data |>
  filter(bat_speed >= 50) 

# View distribution of swing length and bat speed -------------------------

comp_swing_tracking_data |>
  ggplot(aes(x = swing_length)) +
  geom_histogram() +
  theme_bw()

comp_swing_tracking_data |>
  ggplot(aes(x = bat_speed)) +
  geom_histogram() +
  theme_bw()
# slightly skewed left...

comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed)) +
  geom_point(alpha = 0.25) +
  theme_bw()

# Add in the competitive swings color...
comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = is_competitive_swing)) +
  geom_point(alpha = 0.25) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Squared up?
comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed,
             color = squared_up)) +
  geom_point(alpha = 0.25) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, color = squared_up)) +
  geom_density() +
  ggthemes::scale_color_colorblind() +
  theme_bw()


# How does this vary by count ---------------------------------------------

comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, color = as.factor(strikes))) +
  geom_density() +
  facet_wrap(~balls, ncol = 1) +
  theme_bw()

comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, color = as.factor(strikes))) +
  stat_ecdf() +
  facet_wrap(~balls, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")
# See a bit of an ordering effect with 2 strikes as shorter swings, don't really
# see an interaction between strikes and balls

# Flip it...
comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, color = as.factor(balls))) +
  stat_ecdf() +
  facet_wrap(~strikes, ncol = 3) +
  theme_bw() +
  theme(legend.position = "bottom")
#  Looks like 0 balls are shorter swings, with the longest swings on 3 balls
#  (which makes sense - but again, not an interaction)


# How does this change by pitch type? -------------------------------------

# Look at different pitch type variables:
table(bat_tracking_data$pitch_type)
#      CH    CS    CU    EP    FA    FC    FF    FO    FS    KC    KN    SI    SL    ST    SV 
# 1  9373     1  4515    36    73  7142 26659    22  3008  1380   106 13085 14272  4631   245 

table(bat_tracking_data$pitch_name)
#   4-Seam Fastball        Changeup       Curveball          Cutter          Eephus 
# 1           26659            9373            4515            7142              36 
# Forkball   Knuckle Curve     Knuckleball           Other          Sinker          Slider 
#       22            1380             106              73           13085           14272 
# Slow Curve          Slurve    Split-Finger         Sweeper 
#          1             245            3008            4631 

library(ggridges)
comp_swing_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(y = pitch_name, x = swing_length)) +
  geom_density_ridges() +
  theme_bw()
# So fastball/sinker/cutters are shifted to to be shorter length than offspeed/breaking balls

# What about for bat speed?
comp_swing_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(y = pitch_name, x = bat_speed)) +
  geom_density_ridges() +
  theme_bw()
# Hmmm not as stark as the swing length...

comp_swing_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(x = swing_length, y = bat_speed)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~pitch_name, ncol = 3) +
  theme_bw()
# Consistent with the ridges - definitely observe more longer swings for non fastballs,
# but is it that due to the swing itself or the selection of who is receiving those pitches,
# ie, batters with longer swings are more likely to see non fastballs

# Thought: this probably has more to do with speed than anything right?
# are batters more likely to miss slower pitches out in front, thus longer swings?


# Explore relationship with pitch speed -----------------------------------

comp_swing_tracking_data |>
  filter(!is.na(effective_speed), effective_speed > 0) |>
  ggplot(aes(x = effective_speed, y = swing_length)) +
  geom_point(alpha = 0.25) +
  theme_bw()

comp_swing_tracking_data |>
  filter(!is.na(effective_speed), effective_speed > 0,
         squared_up) |>
  ggplot(aes(x = effective_speed, y = swing_length)) +
  geom_point(alpha = 0.25) +
  theme_bw()

comp_swing_tracking_data |>
  filter(!is.na(effective_speed), effective_speed > 0,
         pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(x = effective_speed, y = swing_length)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~pitch_name, ncol = 3) +
  theme_bw()
# Hmmm can see a bit of a relationship, where the faster it is - the longer the
# swing (although that is pretty weak and likely confounded...)

# Explore relationship with pitch location --------------------------------

# First based on vertical release
comp_swing_tracking_data |>
  ggplot(aes(x = swing_length, y = plate_z)) +
  geom_point(alpha = 0.25) +
  theme_bw()
# This makes sense - pitches higher in the zone are shorter swings

comp_swing_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(x = swing_length, y = plate_z)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~pitch_name, ncol = 3) +
  theme_bw()
# Pretty consistent across pitch types

# Next by horizontal - with inside is negative and outside is positive
comp_swing_tracking_data |>
  ggplot(aes(y = swing_length, x = plate_x_ref)) +
  geom_point(alpha = 0.25) +
  labs(x = "Horizontal pitch location wrt batter stand (-inside/+outside)",
       y = "Swing length") +
  theme_bw()
# ever so slight longer swings are more outside pitches...

# Explore batter level  ---------------------------------------------------

# Do this in a lazy way first without removing the clear check swings
batter_summary <- comp_swing_tracking_data |>
  group_by(player_name, batter) |>
  summarize(n_swings = n(),
            ave_bs = mean(bat_speed, na.rm = TRUE),
            var_bs = var(bat_speed, na.rm = TRUE),
            ave_length = mean(swing_length, na.rm = TRUE),
            var_length = var(swing_length, na.rm = TRUE),
            .groups = "drop")

# View ecdf for n_swings
batter_summary |>
  ggplot(aes(x = n_swings)) +
  stat_ecdf() +
  theme_bw()
# Hmm, drop to have at least 50 swings

batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = ave_bs)) +
  geom_histogram() +
  theme_bw()

batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = ave_length)) +
  geom_histogram() +
  theme_bw()
# Interesting....

batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = ave_length, y = ave_bs)) +
  geom_point(alpha = 0.5) +
  theme_bw()

# And now variances?
batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = var_bs)) +
  geom_histogram() +
  theme_bw()

batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = var_length)) +
  geom_histogram() +
  theme_bw()
# Both of these are behaving better than I expected...

# Hmm interesting, not really related to each other
batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = var_bs, y = ave_bs)) +
  geom_point(alpha = 0.5) +
  theme_bw()

batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = var_length, y = var_bs)) +
  geom_point(alpha = 0.5) +
  theme_bw()

batter_summary |>
  filter(n_swings >= 50) |>
  ggplot(aes(x = var_length, y = ave_length)) +
  geom_point(alpha = 0.5) +
  theme_bw()

batter_summary |>
  filter(n_swings >= 200) |>
  ggplot(aes(x = var_length, y = ave_length)) +
  geom_text(aes(label = player_name,
                color = ave_bs)) +
  scale_color_viridis_c() +
  labs(x = "Sample variance of swing length",
       y = "Average swing length",
       color = "Average bat speed",
       caption = "Data courtesy of baseballsavant",
       title = "Hmmmm....",
       subtitle = "Displaying batters w/ at least 200 swings (min 50 mph bat speed) through 5/21") +
  theme_bw() +
  theme(legend.position = "bottom")


batter_summary |>
  filter(n_swings >= 200) |>
  ggplot(aes(x = var_length, y = ave_bs)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = player_name)) +
  scale_color_viridis_c() +
  labs(x = "Sample variance of swing length",
       y = "Average bat speed",
       caption = "Data courtesy of baseballsavant",
       title = "Hmmmm....",
       subtitle = "Displaying batters w/ at least 200 swings (min 50 mph bat speed) through 5/21") +
  theme_bw() +
  theme(legend.position = "bottom")


batter_summary |>
  filter(n_swings >= 200) |>
  ggplot(aes(x = var_bs, y = ave_bs)) +
  geom_text(aes(label = player_name,
                color = ave_length)) +
  scale_color_viridis_c() +
  labs(x = "Sample variance of bat speed",
       y = "Average bat speed",
       color = "Average swing length",
       caption = "Data courtesy of baseballsavant",
       title = "Hmmmm....",
       subtitle = "Displaying batters w/ at least 200 swings (min 50 mph bat speed) through 5/21") +
  theme_bw() +
  theme(legend.position = "bottom")

batter_summary |>
  filter(n_swings >= 200) |>
  ggplot(aes(x = var_length, y = var_bs)) +
  #geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = player_name,
                color = ave_bs)) +
  scale_color_viridis_c() +
  labs(x = "Sample variance of swing length",
       y = "Sample variance of bat speed",
       color = "Average bat speed",
       caption = "Data courtesy of baseballsavant",
       title = "Hmmmm....",
       subtitle = "Displaying batters w/ at least 200 swings (min 50 mph bat speed) through 5/21") +
  theme_bw() +
  theme(legend.position = "bottom")
