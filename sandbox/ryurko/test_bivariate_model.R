# PURPOSE: Explore modeling the swing length and speed with a bivariate model

library(tidyverse)

# Read in the bat tracking data -------------------------------------------

# Follow the pre-processing from Scott's code:
bat_tracking_data <- read_rds("data/bat_tracking_0521.rds") |>
  dplyr::mutate(
    # First we have to calculate the speed of the pitch at the plate, to reproduce squared-up rate
    cx = release_pos_x,
    cy = release_pos_y,
    cz = release_pos_z,
    # y0 is the value of y at the time when y = 50 (so it is defintionally 50)
    y0 = 50,
    # t0 is the time corresponding to vx0, vy0, vz0, x0, z0 (i.e. the time when y = 50)
    # Calculate time from y0 to release point, and then negate that time
    t0 = -(-vy0 - sqrt(vy0^2 - 4 * (ay / 2) * (y0 - cy))) / (2 * (ay / 2)),
    # Backtrack velocities by t0 time
    bx = vx0 + (-t0) * ax,
    by = vy0 + (-t0) * ay,
    bz = vz0 + (-t0) * az,
    plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
    plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
    plate_speed = 0.6818182 * sqrt((ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2),
    squared_up = ifelse(
      test = description == "hit_into_play" & !is.na(launch_speed),
      yes = (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
      no = FALSE
    ),
    # Infer spray angle from hit coordinates, and reflect spray angle and plate x for LHBs
    spray_angle = (atan((hc_x - 125) / (205 - hc_y))) * 180 / pi, # radian to degree
    spray_angle_ref = ifelse(stand == "L", -spray_angle, spray_angle),
    plate_x_ref = ifelse(stand == "L", -plate_x, plate_x),
    is_bunt = stringr::str_detect(description, "bunt") |
      (description == "hit_into_play" & stringr::str_detect(des, " bunt")),
    
    pitch_type_cat = dplyr::case_when(
      pitch_type %in% c("FF", "SI") ~ "fastball",
      pitch_type %in% c("SL", "FC", "CU", "ST", "KC", "SV") ~ "breaking",
      pitch_type %in% c("CH", "FS") ~ "offspeed",
    ),
    count = paste0(balls, "-", strikes),
    xwoba = estimated_woba_using_speedangle,  # just creating a shorter variable name here
    batter_id = paste0(batter, stand)   # treat switch hitters as two players
  ) |>
  dplyr::filter(
    !is.na(swing_length),
    !is.na(pitch_type_cat),
    !is_bunt,
    bat_speed > 50
  ) |>
  dplyr::ungroup()

player_name <- bat_tracking_data |>
  dplyr::distinct(batter_id, player_name)

squared_up_data <- bat_tracking_data |>
  dplyr::filter(squared_up) |>
  dplyr::group_by(batter_id) |>
  dplyr::filter(dplyr::n() > 30) |>
  dplyr::ungroup()


# Fit the lme4 versions ---------------------------------------------------

squared_up_bat_speed <- lme4::lmer(
  bat_speed ~ balls + strikes + plate_x_ref + plate_z + (1 + strikes + plate_x_ref + plate_z | batter_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = squared_up_data
)

squared_up_swing_length <- lme4::lmer(
  swing_length ~ balls + strikes + plate_x_ref + plate_z + (1 + strikes + plate_x_ref + plate_z | batter_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = squared_up_data
)


# Fit the bivariate version with nlme -------------------------------------

# Follow the example here: https://stats.oarc.ucla.edu/r/faq/multivariate-random-coefficient-model/
long_squared_up_data <- squared_up_data |>
  dplyr::select(batter_id, balls, strikes, plate_x_ref, plate_z, 
                bat_speed, swing_length) |>
  pivot_longer(cols = bat_speed:swing_length,
               names_to = "response", values_to = "value") |>
  # Create the two indicators:
  mutate(is_speed = as.numeric(response == "bat_speed"),
         is_length = as.numeric(response == "swing_length"))

library(nlme)
bivariate_model <- # Remove intercept to separate by variable
  lme(value ~ 0 + is_speed + is_speed:balls + is_speed:strikes + is_speed:plate_x_ref + is_speed:plate_z + 
        is_length + is_length:balls + is_length:strikes + is_length:plate_x_ref + is_length:plate_z,
      random = ~ 0 + is_speed + is_speed:balls + is_speed:strikes + is_speed:plate_x_ref + is_speed:plate_z + 
        is_length + is_length:balls + is_length:strikes + is_length:plate_x_ref + is_length:plate_z | batter_id, 
      data = long_squared_up_data, weights = varIdent(form = ~1 | response), 
      control = lmeControl(maxIter = 200, msMaxIter = 200, 
                           niterEM = 50, msMaxEval = 400))
# Not sure this is going to be worth it for lack of gains we'll have from it...




