
# Preliminaries ----

fig_mode <- "light"
fig_type <- "pdf"
fig_suffix <- glue::glue("{ifelse(fig_mode == 'dark', '_dark', '')}.{fig_type}")
color_fg <- sputil::color("fg", mode = fig_mode)
color_bg <- sputil::color("bg", mode = fig_mode)
color_base2 <- sputil::color("base2", mode = fig_mode)
color_blue <- sputil::color("blue", mode = fig_mode)

# Load models and data from file ----

intention_model_bat_speed <- readRDS("models/intent_bat_speed_full.rds")
intention_model_swing_length <- readRDS("models/intent_swing_length_full.rds")
causal_model <- readRDS("models/causal.rds")

approach <- swingfastslow::get_intention_model_summary(
  intention_model_bat_speed = intention_model_bat_speed,
  intention_model_swing_length = intention_model_swing_length
)
fixef_strikes_bat_speed <- mean(
  tidybayes::gather_draws(intention_model_bat_speed, b_strikes)$.value
)
fixef_strikes_swing_length <- mean(
  tidybayes::gather_draws(intention_model_swing_length, b_strikes)$.value
)

hit_outcome_model <- readRDS("models/hit_outcome_model.rds")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
linear_weight <- read.csv("models/linear_weight.csv")
player <- data.table::fread("data/player.csv")
data <- data.table::fread("data/baseballsavant.csv") |>
  dplyr::filter(balls < 4, strikes < 3) |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
  swingfastslow::predict_pitch_hit_outcomes(
    pitch_outcome_model = pitch_outcome_model,
    hit_outcome_model = hit_outcome_model
  )
swing <- data |>
  dplyr::filter(!is.na(bat_speed), !is.na(swing_length)) |>
  swingfastslow::remove_partial_swings() |>
  swingfastslow::recreate_squared_up()


# 1. Introduction ----

data_plot <- swing |>
  dplyr::group_by(batter_id) |>
  dplyr::mutate(relative_bat_speed = bat_speed - mean(bat_speed)) |>
  dplyr::ungroup()

{
  sputil::open_device(paste0("figures/counterintuitive", fig_suffix), height = 3, width = 6)
  plot <- data_plot |>
    dplyr::mutate(squared_up = ifelse(squared_up, "Yes", "No")) |>
    ggplot2::ggplot(ggplot2::aes(squared_up, relative_bat_speed)) +
    ggplot2::geom_violin(fill = NA, color = color_blue) +
    ggplot2::stat_summary(
      mapping = ggplot2::aes(shape = "Average"),
      fun = mean,
      geom = "point",
      size = 2,
      color = color_blue,
      fill = color_blue,
      alpha = 0.5
    ) +
    ggplot2::scale_shape_manual(ggplot2::element_blank(), values = c("Average" = 21)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Squared Up",
      y = "Bat Speed (mph), Relative to Batter Average"
    ) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.9, 0.5))
  print(plot)
  dev.off()
}

create_swing_diagram <- function(rotation_angle, ball_loc, label) {

  bat <- magick::image_read("figures/bat.png") |>
    magick::image_background("none") |>
    magick::image_rotate(rotation_angle) |>
    grid::rasterGrob()

  bat_box <- matrix(c(0, 0, 8, 8, -0.5, 0.5, -0.5, 0.5), nrow = 2, ncol = 4, byrow = TRUE)
  radians <- -rotation_angle / 180 * pi
  rotation_matrix <- matrix(
    data = c(cos(radians), sin(radians), -sin(radians), cos(radians)),
    nrow = 2,
    ncol = 2
  )
  bat_box_rot <- rotation_matrix %*% bat_box
  bat_path <- tibble::tibble(
    angle = seq(from = -90, to = -rotation_angle),
    x = 8 * cos(angle / 180 * pi),
    y = 8 * sin(angle / 180 * pi),
  )

  swing_plot <- tibble::tibble(x = ball_loc[1], y = ball_loc[2]) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = ball_loc[1], yend = Inf),
      color = color_fg,
      linetype = "dotted"
    ) +
    ggplot2::geom_point(size = 5, shape = 21, color = color_fg, fill = "white") +
    ggplot2::geom_path(data = bat_path, ggplot2::aes(x = x, y = y), color = color_fg) +
    ggplot2::annotation_custom(
      bat,
      xmin = min(bat_box_rot[1, ]),
      xmax = max(bat_box_rot[1, ]),
      ymin = min(bat_box_rot[2, ]),
      ymax = max(bat_box_rot[2, ])
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 9), ylim = c(-5, 5)) +
    ggplot2::labs(
      x = label,
      y = ggplot2::element_blank()
    ) +
    sputil::theme_sleek(mode = fig_mode) +
    sputil::remove_axes()

  return(swing_plot)
}

{
  sputil::open_device(paste0("figures/swing_late", fig_suffix), height = 2, width = 2)
  plot <- create_swing_diagram(
    rotation_angle = 20,
    ball_loc = c(6, -1.1),
    label = "LATE Swing"
  )
  print(plot)
  dev.off()
}

{
  sputil::open_device(paste0("figures/swing_early", fig_suffix), height = 2, width = 2)
  plot <- create_swing_diagram(
    rotation_angle = -20,
    ball_loc = c(6, 3.3),
    label = "EARLY Swing"
  )
  print(plot)
  dev.off()
}


# 4 Results ----

# 4.1 Intention Model ----

approach_interpreted <- approach |>
    # Add in the fixed effect for strikes
    dplyr::mutate(
      # Convert swing length to inches
      strikes_swing_length = 12 * (strikes_swing_length + fixef_strikes_swing_length),
      strikes_bat_speed = strikes_bat_speed + fixef_strikes_bat_speed
    ) 

{
  sputil::open_device(paste0("figures/approach", fig_suffix), height = 4, width = 5)
  plot <- approach_interpreted |>
    ggplot2::ggplot(ggplot2::aes(x = strikes_swing_length, y = strikes_bat_speed)) +
    ggplot2::geom_point(color = color_blue, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    ggplot2::coord_cartesian(xlim = c(-3, 0), ylim = c(-2, 0)) +
    ggplot2::labs(
      x = "Swing Length Reduction per Strike (inches)",
      y = "Bat Speed Reduction per Strike (mph)"
    ) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}


# 4.2 Causal Model ----

display_coef <- function(fit, scale) {
  paste0(
    "$",
    sprintf("%.3f", scale * summary(fit)$coefficients[-1, "Estimate"]),
    " \\pm",
    sprintf("%.3f", scale * summary(fit)$coefficients[-1, "Std. Error"]),
    "$"
  )
}

tibble::tibble(
  rowname = c("Bat Speed Approach (mph)", "Swing Length Approach (inches)"),
  # Scale swing length effect by 1/12 to reflect inches rather than feet
  contact = display_coef(causal_model$fit_contact, scale = c(1, 1 / 12)),
  fair = display_coef(causal_model$fit_fair, scale = c(1, 1 / 12)),
  hit = display_coef(causal_model$fit_hit, scale = c(1, 1 / 12))
) |>
  tibble::column_to_rownames() |>
  sputil::write_latex_table("tables/causal_model.tex", include.rownames = TRUE)

data_with_approach <- data |>
  dplyr::filter(strikes == 2) |>
  dplyr::mutate(batter_side_id = paste0(batter_id, bat_side)) |>
  dplyr::inner_join(approach, by = "batter_side_id") |>   # some batters not in intention model
  dplyr::mutate(
    approach_bat_speed = strikes * strikes_bat_speed,
    approach_swing_length = strikes * strikes_swing_length
  )

data_with_approach_adjusted <- data_with_approach |>
  dplyr::mutate(
    prob_contact_adj = predict(
      object = causal_model$fit_contact,
      newdata = data_with_approach,
      type = "response"
    ),
    pred_hit_adj = predict(
      object = causal_model$fit_hit,
      newdata = data_with_approach,
      type = "response"
    )
  )

approach_effect_summary <- data_with_approach_adjusted |>
  dplyr::group_by(batter_side_id) |>
  dplyr::summarize(
    strikes_bat_speed = mean(strikes_bat_speed) + fixef_strikes_bat_speed,
    strikes_swing_length = mean(strikes_swing_length) + fixef_strikes_swing_length,
    prob_contact_diff = mean(prob_contact_adj - prob_contact),
    pred_hit_diff = mean(pred_hit_adj - pred_hit),
    .groups = "drop"
  ) |>
  # Perform re-centering to compensate for model drift
  dplyr::mutate(
    prob_contact_diff = prob_contact_diff - mean(prob_contact_diff, na.rm = TRUE),
    pred_hit_diff = pred_hit_diff - mean(pred_hit_diff, na.rm = TRUE)
  )

height <- 3
width <- 4

{
  sputil::open_device(paste0("figures/bat_speed_contact", fig_suffix),
    height = height,
    width = width
  )
  bat_speed_contact_plot <- approach_effect_summary |>
    ggplot2::ggplot(ggplot2::aes(x = 2 * strikes_bat_speed, y = prob_contact_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    ggplot2::geom_point(col = color_blue, alpha = 0.5) +
    ggplot2::labs(
      x = "2-Strike Bat Speed Delta (mph)",
      y = "Contact Effect in 2-Strike Counts"
    ) +
    ggplot2::scale_y_continuous(breaks = c(-.04, 0, .04), labels = c("-4%", "0%", "+4%")) +
    ggplot2::coord_cartesian(ylim = c(-.07, .07)) +
    sputil::theme_sleek(mode = fig_mode)
  print(bat_speed_contact_plot)
  dev.off()
}

{
  sputil::open_device(paste0("figures/bat_speed_power", fig_suffix),
    height = height,
    width = width
  )
  bat_speed_contact_plot <- approach_effect_summary |>
    # 1.242 is wOBA scaling factor for 2024 (https://www.fangraphs.com/guts.aspx)
    ggplot2::ggplot(ggplot2::aes(x = 2 * strikes_bat_speed, y = 1.242 * pred_hit_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    ggplot2::geom_point(col = color_blue, alpha = 0.5) +
    ggplot2::labs(
      x = "2-Strike Bat Speed Delta (mph)",
      y = "xwOBA Effect in 2-Strike Counts"
    ) +
    ggplot2::scale_y_continuous(breaks = c(-.04, 0, .04), labels = c("-.040", ".000", "+.040")) +
    ggplot2::coord_cartesian(ylim = c(-.07, .07)) +
    sputil::theme_sleek(mode = fig_mode)
  print(bat_speed_contact_plot)
  dev.off()
}

# Caculate the run value of different approaches

# Identify the approaches for which we want to calculate value
approach_matrix <- approach |>
  dplyr::select(strikes_swing_length, strikes_bat_speed) |>
  as.matrix()
approach_cluster <- kmeans(approach_matrix, centers = 50)$centers
approach_chull <- approach_matrix[chull(approach_matrix), ]
approach_tibble <- tibble::as_tibble(rbind(c(0, 0), approach_cluster, approach_chull))
approach_list <- split(approach_tibble, f = 1:nrow(approach_tibble))

approach_value_list <- pbapply::pblapply(
  X = approach_list,
  FUN = swingfastslow::evaluate_approach,
  pred_outcome_pitch = data,
  causal_model = causal_model,
  linear_weight = linear_weight
)
approach_value <- do.call(dplyr::bind_rows, approach_value_list) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # We're going to say that a zero approach is definitionally zero runs above average
    runs = 500 * (runs - runs[1]),                        # 500 PA scale
    # Make approach relative to no adjustment, instead of being relative to average
    strikes_swing_length = 12 * (strikes_swing_length + fixef_strikes_swing_length),
    strikes_bat_speed = strikes_bat_speed + fixef_strikes_bat_speed
  )

approach_runs_model <- mgcv::gam(
  runs ~ s(strikes_bat_speed, strikes_swing_length),
  data = approach_value
)

batter <- data |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    batter_name_comma = stringr::str_locate(batter_name, ",")[, "start"],
    batter_name_last = substring(batter_name, 1, batter_name_comma - 1),
    batter_initial_first = substring(batter_name, batter_name_comma + 2, batter_name_comma + 2),
    batter_name_short = paste(batter_initial_first, batter_name_last)
  ) |>
  dplyr::distinct(batter_side_id, batter_name_short)

approach_grid <- approach |>
  with(
    expand.grid(
      strikes_swing_length = seq(-3.5, 0, length = 200),
      strikes_bat_speed = seq(-2.5, 0, length = 200)
    )
  )

approach_grid_with_pred <- approach_grid |>
  dplyr::mutate(runs = predict(approach_runs_model, newdata = approach_grid)) |>
  # Avoid showing predictions that are extrapolating too far
  dplyr::filter(runs >= min(approach_value$runs), runs <= max(approach_value$runs))

{
  sputil::open_device(paste0("figures/approach_run_value", fig_suffix), height = 4, width = 5)
  plot <- approach_grid_with_pred |>
    ggplot2::ggplot(ggplot2::aes(strikes_swing_length, strikes_bat_speed)) +
    ggplot2::geom_raster(ggplot2::aes(fill = runs)) +
    ggplot2::geom_point(data = approach_interpreted, color = "black", alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    ggplot2::scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
    ggplot2::labs(
#      title = switch(medium,
#        beamer = ggplot2::element_blank(),
#        article = "Is it good to modulate your swing by count?"
#      ),
#      subtitle = "Estimated run value of approach applied to average batter",
      x = "Swing Length Reduction per Strike (inches)",
      y = "Bat Speed Reduction per Strike (mph)",
      fill = "Runs per 500 PA"
    ) +
    ggplot2::coord_cartesian(xlim = c(-3, 0), ylim = c(-2, 0)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.18, 0.71))
  print(plot)
  dev.off()
}

approach_interpreted |>
  dplyr::mutate(player_id = as.integer(substring(batter_side_id, 1, 6))) |>
  dplyr::left_join(player, by = "player_id") |>
  dplyr::mutate(runs = predict(approach_runs_model, newdata = approach_interpreted)) |>
  dplyr::arrange(-runs) |>
  dplyr::mutate(rank = 1:dplyr::n()) |>
  dplyr::select(rank, name_full, strikes_bat_speed, strikes_swing_length, runs) |>
  dplyr::slice(c(1:5), dplyr::n() - 4:0) |>
  dplyr::mutate(
    strikes_bat_speed = glue::glue("${sprintf('%.2f', strikes_bat_speed)}$"),
    strikes_swing_length = glue::glue("${sprintf('%.2f', strikes_swing_length)}$"),
    runs = glue::glue("${sprintf('%.2f', runs)}$"),
  ) |>
  sputil::write_latex_table("tables/approach_ranked.tex", buffer_row = 6)


# 4.3 Other Sources of Swing Variation ----

{
  sputil::open_device(paste0("figures/adaptation", fig_suffix), height = 3.5, width = 6)

  batter_side_id_1 <- "663538R"
  batter_name_1 <- "Nico Hoerner"

  batter_side_id_2 <- "665742L"
  batter_name_2 <- "Juan Soto"

  plate_loc_grid <- expand.grid(
    plate_x_ref = seq(from = -1.5, to = 1.5, length = 100),
    plate_z = seq(from = 1, to = 4, length = 100),
    pitcher_id = 682120   # this is the pitcher with the smallest absolute effect on swing length
  ) |>
    dplyr::mutate(balls = 0, strikes = 0)

  plot_data <- plate_loc_grid |>
    dplyr::mutate(
      pred_1 = predict(
        intention_model_swing_length,
        newdata = plate_loc_grid |>
          dplyr::mutate(batter_side_id = batter_side_id_1)
      )[, "Estimate"],
      pred_2 = predict(
        intention_model_swing_length,
        newdata = plate_loc_grid |>
          dplyr::mutate(batter_side_id = batter_side_id_2)
      )[, "Estimate"]
    ) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("pred_")) |>
    dplyr::mutate(
      batter_side_id = dplyr::case_when(
        name == "pred_1" ~ batter_side_id_1,
        name == "pred_2" ~ batter_side_id_2
      ),
      batter_name = dplyr::case_when(
        name == "pred_1" ~ batter_name_1,
        name == "pred_2" ~ batter_name_2
      ),
      plate_x = ifelse(substring(batter_side_id, 7) == "L", -1, 1) * plate_x_ref
    )

  strike_zone <- swing |>
    dplyr::mutate(batter_side_id = paste0(batter_id, bat_side)) |>
    dplyr::filter(batter_side_id %in% c(batter_side_id_1, batter_side_id_2)) |>
    dplyr::mutate(
      batter_name = dplyr::case_when(
        batter_side_id == batter_side_id_1 ~ batter_name_1,
        batter_side_id == batter_side_id_2 ~ batter_name_2
      )
    ) |>
    dplyr::group_by(batter_name) |>
    dplyr::summarize(
      sz_top = mean(strike_zone_top),
      sz_bot = mean(strike_zone_bottom),
      bat_side_x = mean(ifelse(bat_side == "R", -1, 1) * 1.3),
      .groups = "drop"
    )

  adaptation_plot <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = plate_x, y = plate_z, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(
      low = scales::muted("blue"),
      high = scales::muted("red"),
      midpoint = 7
    ) +
    ggplot2::facet_grid(. ~ factor(batter_name, levels = c(batter_name_1, batter_name_2))) +
    ggplot2::geom_rect(
      data = strike_zone,
      mapping = ggplot2::aes(xmin = -17 / 24, xmax = 17 / 24, ymin = sz_bot, ymax = sz_top),
      color = "black",
      alpha = 0.5,
      fill = NA,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = strike_zone,
      mapping = ggplot2::aes(x = bat_side_x, xend = bat_side_x, y = 1.1, yend = 3.9),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 2
    ) +
    ggplot2::labs(
#      subtitle = "Swing length predicted by intention model, assuming 0-0 count",
      fill = "Intended\nSwing\nLength",
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank()
    ) +
    sputil::theme_sleek(mode = fig_mode) +
    sputil::remove_axes() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 12), legend.position = "right")
  print(adaptation_plot)
  dev.off()
}

