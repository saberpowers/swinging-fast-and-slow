# PURPOSE: Initialize utility function from Scott for gathering pitch level data

#' Extract schedule
#' 
#' Extract a table of MLB or AAA games within a specified date range (must be same calendar year).
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @param level character string, "mlb" (default) or "aaa"
#'
#' @return a dataframe of games, with columns `game_id`, `year`, `date`, `team_id_away`, `team_id_home`, `venue_id`
#' 
#' @export
#' 

extract_schedule <- function(start_date, end_date, level = c("mlb", "aaa")) {
  
  if (lubridate::year(start_date) != lubridate::year(end_date)) {
    stop("Please choose `start_date` and `end_date` within the same calendar year")
  }
  level <- match.arg(level)
  
  start <- format(as.Date(start_date), "%m/%d/%Y")
  end <- format(as.Date(end_date), "%m/%d/%Y")
  sport_id <- switch(level, mlb = 1, aaa = 11)
  schedule_filter <- glue::glue("sportId={sport_id}&gameType=R&startDate={start}&endDate={end}")
  endpoint <- glue::glue("http://statsapi.mlb.com:80/api/v1/schedule?{schedule_filter}")
  
  schedule_json <- jsonlite::fromJSON(endpoint, flatten = TRUE)
  
  schedule <- do.call(dplyr::bind_rows, args = schedule_json$dates$games)
  
  # We rely on the resumeDate column to avoid duplicating resumed games, but that column will
  # not be included in `schedule` if there were no resumed games in our timeframe.
  if (is.null(schedule$resumeDate)) {
    schedule$resumeDate <- NA
  }
  
  game <- schedule |>
    # Filter out non-NA resumeDate to get down to one row per game ID
    dplyr::filter(status.detailedState %in% c("Final", "Completed Early"), is.na(resumeDate)) |>
    dplyr::arrange(officialDate) |>
    dplyr::select(
      game_id = gamePk,
      year = season,
      date = officialDate,
      team_id_away = teams.away.team.id,
      team_id_home = teams.home.team.id,
      venue_id = venue.id
    )
  
  return(game)
}

#' Download MLB data from baseballsavant.mlb.com
#' 
#' Loop over five days at a time to download data from the Statcast search API at Baseball Savant.
#' This includes swing tracking data not available through the MLB statsapi.
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' 
#' @return a dataframe with 94 columns and one row per pitch, with all available data
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'    data_statsapi <- sabRmetrics::download_statsapi(
#'      start_date = "2024-07-01",
#'      end_date = "2024-07-01"
#'    )
#' }
#' 

download_baseballsavant <- function(start_date, end_date, cl = NULL) {
  
  # Trim start date and end date to range of actual games
  schedule <- extract_schedule(start_date, end_date, level = "mlb")
  start_date <- min(schedule$date)
  end_date <- max(schedule$date)
  
  base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details"
  
  # Split the dates into 5-day chunks. The Savant API will return at most 25,000 rows. Assuming 300
  # pitches per game, a day with 15 games will have 4,500 pitches. We can safely download 5 days of
  # data, but more days would risk hitting the 25,000-row limit.
  days <- as.numeric(as.Date(end_date) - as.Date(start_date))
  start_date_seq <- as.Date(start_date) + seq(from = 0, by = 5, to = days)
  
  data_list <- pbapply::pblapply(
    X = start_date_seq,
    FUN = function(start_date) {
      end_date <- start_date + 4
      base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details"
      url <- glue::glue("{base_url}&game_date_gt={start_date}&game_date_lt={end_date}")
      data <- read.csv(url(url))
      if (nrow(data) == 25000) {
        warning(
          glue::glue("Exactly 25,000 rows returned for {start_date} to {end_date}")
        )
      }
      return(data)
    },
    cl = cl
  )
  
  data <- do.call(dplyr::bind_rows, args = data_list) |>
    tibble::as_tibble() |>
    # re-define columns as needed to match statsapi
    dplyr::mutate(
      event_index = at_bat_number - 1,
      half_inning = dplyr::case_when(
        inning_topbot == "Top" ~ "top",
        inning_topbot == "Bot" ~ "bottom"
      )
    ) |>
    # re-order and re-name columns (but don't drop any)
    dplyr::select(
      # pitch identifiers (for joining on pitch table from statsapi)
      game_id = game_pk,
      year = game_year,
      event_index,
      pitch_number,
      # player and team identifiers
      home_team,
      away_team,
      batter_id = batter,
      bat_side = stand,
      batter_name = player_name,
      pitcher_id = pitcher,
      pitch_hand = p_throws,
      pre_runner_1b_id = on_1b,
      pre_runner_2b_id = on_2b,
      pre_runner_3b_id = on_3b,
      fielder_2_id = fielder_2,
      fielder_3_id = fielder_3,
      fielder_4_id = fielder_4,
      fielder_5_id = fielder_5,
      fielder_6_id = fielder_6,
      fielder_7_id = fielder_7,
      fielder_8_id = fielder_8,
      fielder_9_id = fielder_9,
      # context
      inning,
      half_inning,
      outs = outs_when_up,
      balls,
      strikes,
      if_fielding_alignment,
      of_fielding_alignment,
      # pitch tracking
      pitch_type,
      ## here are the features you need to recreate the full quadratic trajectory of the pitch
      ax,
      ay,
      az,
      vx0,
      vy0,
      vz0,
      release_pos_x,
      release_pos_y,
      release_pos_z,
      ## here are the highly interpretable features that are functions of the quadratic trajectory
      release_speed,
      extension = release_extension,
      effective_speed,
      pfx_x,
      pfx_z,
      plate_x,
      plate_z,
      zone,
      ## here are the additional features that can't be derived from the quadratic trajectory
      release_spin_rate,
      spin_axis,
      strike_zone_top = sz_top,
      strike_zone_bottom = sz_bot,
      # swing tracking
      bat_speed,
      swing_length,
      # batted ball tracking
      launch_speed,
      launch_angle,
      expected_woba = estimated_woba_using_speedangle,
      expected_babip = estimated_ba_using_speedangle,
      hit_coord_x = hc_x,
      hit_coord_y = hc_y,
      hit_distance_sc,
      bb_type,
      hit_location,
      # outcome
      type,
      description,
      events,
      woba_denom,
      woba_value,
      babip_value,
      iso_value,
      delta_run_exp,
      delta_home_win_exp,
      # retain all other columns, but keep them last because they don't have a clear use case
      dplyr::everything()
    )
  
  return(data)
}

