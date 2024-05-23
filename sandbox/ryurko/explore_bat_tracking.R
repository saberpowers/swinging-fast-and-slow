# PURPOSE: Explore bat tracking data - just basic exploration of distributions,
#          relationships with pitch types and locations

library(tidyverse)

# Read in the bat tracking data -------------------------------------------

bat_tracking_data <- read_rds("data/bat_tracking_0521.rds")

# View distribution of swing length and bat speed -------------------------

bat_tracking_data |>
  ggplot(aes(x = swing_length)) +
  geom_histogram() +
  theme_bw()
# Cleary multimodal distribution - where the small mode are likely swords / checkswings

bat_tracking_data |>
  ggplot(aes(x = bat_speed)) +
  geom_histogram() +
  theme_bw()
# clearly left skewed...

bat_tracking_data |>
  ggplot(aes(x = swing_length, y = bat_speed)) +
  geom_point(alpha = 0.25) +
  theme_bw()
# Pretty interesting - looks like the bottom left cluster are check swings / swords?

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
bat_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(y = pitch_name, x = swing_length)) +
  geom_density_ridges() +
  theme_bw()
# So fastball/sinker/cutters are shifted to to be shorter length than offspeed/breaking balls

# What about for bat speed?
bat_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(y = pitch_name, x = bat_speed)) +
  geom_density_ridges() +
  theme_bw()
# Hmmm not as stark as the swing length...

bat_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(x = swing_length, y = bat_speed)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~pitch_name, ncol = 3) +
  theme_bw()
# Consistent with the ridges - definitely observe more longer swings for non fastballs,
# but is it that due to the swing itself or the selection of who is receiving those pitches,
# ie, batters with longer swings are more likely to see non fastballs

# Explore relationship with pitch location --------------------------------

# First based on vertical release
bat_tracking_data |>
  ggplot(aes(x = swing_length, y = plate_z)) +
  geom_point(alpha = 0.25) +
  theme_bw()
# This makes sense - pitches higher in the zone are shorter swings

bat_tracking_data |>
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST")) |>
  mutate(pitch_name = fct_infreq(pitch_name)) |>
  ggplot(aes(x = swing_length, y = plate_z)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~pitch_name, ncol = 3) +
  theme_bw()
# Pretty consistent across pitch types

# Next by horizontal - with inside is negative and outside is positive
bat_tracking_data |>
  mutate(plate_x = ifelse(stand == "L", -plate_x, plate_x)) |>
  ggplot(aes(y = swing_length, x = plate_x)) +
  geom_point(alpha = 0.25) +
  labs(x = "Horizontal pitch location wrt batter stand (-inside/+outside)",
       y = "Swing length") +
  theme_bw()
# ever so slight longer swings are more outside pitches...

# Explore batter level  ---------------------------------------------------

# Do this in a lazy way first without removing the clear check swings
batter_summary <- bat_tracking_data |>
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
       subtitle = "Displaying batters w/ at least 200 swings through 5/21, w/o removing check-swings/swords") +
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
       subtitle = "Displaying batters w/ at least 200 swings through 5/21, w/o removing check-swings/swords") +
  theme_bw() +
  theme(legend.position = "bottom")
