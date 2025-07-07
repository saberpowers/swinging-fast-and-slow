
# Download pitch-by-pitch tracking and results data ----

cluster <- parallel::makeCluster(parallel::detectCores())
baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-04-03",  # first day of swing tracking data
  end_date = "2024-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)

baseballsavant |>
  dplyr::select(
    game_date, game_id, event_index, pitch_number, batter_id, bat_side, batter_name,
    pitcher_id, inning, half_inning, outs, balls, strikes,
    ax, ay, az, vx0, vy0, vz0, release_pos_x, release_pos_y, release_pos_z,
    pitch_type, release_speed, plate_x, plate_z, strike_zone_top, strike_zone_bottom,
    bat_speed, swing_length, launch_speed, launch_angle, hit_coord_x, hit_coord_y,
    description, des
  ) |>
  data.table::fwrite(file = "data/baseballsavant.csv")


# Download player data ----

player <- sabRmetrics::download_player(year = 2024, level = "MLB")
player |>
  dplyr::select(player_id, name_full, birth_date, bat_side, throw_hand, primary_position) |>
  data.table::fwrite(file = "data/player.csv")
