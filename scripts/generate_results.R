
medium <- "beamer"  # options: "article", "beamer" or "twitter"


# Load models and data from file ----

devtools::load_all("package/swingfastslow")
intention_model <- readRDS("models/intention.rds")
approach_model <- readRDS("models/approach.rds")
hit_outcome_model <- readRDS("models/hit_outcome_model.rds")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
linear_weight <- read.csv("models/linear_weight.csv")
data <- data.table::fread("data/baseballsavant.csv") |>
  dplyr::filter(!is.na(balls), !is.na(strikes)) |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
  predict_pitch_hit_outcomes(
    pitch_outcome_model = pitch_outcome_model,
    hit_outcome_model = hit_outcome_model
  )
swing <- data |>
  dplyr::filter(!is.na(bat_speed), !is.na(swing_length)) |>
  remove_partial_swings() |>
  recreate_squared_up()


# Preliminaries ----

color_fg <- "#93a1a1"
color_bg <- "#002B37"
color_base <- "#073642"
color_base2 <- "#eee8d5"
color_red <- "#dc322f"
color_orange <- "#cb4b16"
color_yellow <- "#b58900"
color_green <- "#859900"
color_blue <- "#268bd2"

theme_medium <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = color_fg),
      axis.ticks = ggplot2::element_line(color = color_fg),
      axis.text = ggplot2::element_text(color = color_fg),
      strip.text = ggplot2::element_text(color = color_fg),
      text = ggplot2::element_text(color = color_fg),
      legend.background = ggplot2::element_rect(fill = NA),
      panel.background = ggplot2::element_rect(fill = color_bg),
      plot.background = ggplot2::element_rect(fill = color_bg, color = color_bg),
      strip.background = ggplot2::element_rect(fill = color_bg, color = color_bg)
    )
}

remove_axes <- function() {
  ggplot2::theme(
    axis.line.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
}

open_device <- function(filename,
                        medium = c("article", "beamer", "twitter"),
                        height = 7,
                        width = 7,
                        ppi_png = 300) {
  medium <- match.arg(medium)
  if (medium %in% c("article", "beamer")) {
    pdf(glue::glue("figures/{medium}/{filename}.pdf"), height = height, width = width)
  } else if (medium == "twitter") {
    png(glue::glue("figures/{medium}/{filename}.pdf"),
      height = ppi_png * height,
      width = ppi_png * width,
      ppi = ppi_png
    )
  }
  invisible()
}


# Counterintuitive results ----

data_plot <- swing |>
  dplyr::group_by(batter_id) |>
  dplyr::mutate(relative_bat_speed = bat_speed - mean(bat_speed)) |>
  dplyr::ungroup()

{
  open_device("counterintuitive", medium, height = 3, width = 6)
  counterintuitive_plot <- data_plot |>
    dplyr::mutate(squared_up = ifelse(squared_up, "Yes", "No")) |>
    ggplot2::ggplot(ggplot2::aes(squared_up, relative_bat_speed)) +
    ggplot2::geom_violin(fill = NA, color = color_fg) +
    ggplot2::stat_summary(
      mapping = ggplot2::aes(shape = "Average"),
      fun = mean,
      geom = "point",
      size = 2,
      color = color_fg,
      fill = color_fg,
      alpha = 0.5
    ) +
    ggplot2::scale_shape_manual(ggplot2::element_blank(), values = c("Average" = 21)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      subtitle = '"Squared Up" = good contact (exit velocity > 80% of theoretical max)',
      x = "Squared Up",
      y = "Bat Speed (mph), Relative to Batter Average"
    ) +
    theme_medium() +
    ggplot2::theme(legend.position = c(0.9, 0.5))
  print(counterintuitive_plot)
  dev.off()
}


# Swing diagrams ----

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
      color = color_base2,
      linetype = "dotted"
    ) +
    ggplot2::geom_point(size = 5, color = color_base2) +
    ggplot2::geom_path(data = bat_path, ggplot2::aes(x = x, y = y), color = color_fg) +
    ggplot2::coord_fixed() +
    ggplot2::annotation_custom(
      bat,
      xmin = min(bat_box_rot[1, ]),
      xmax = max(bat_box_rot[1, ]),
      ymin = min(bat_box_rot[2, ]),
      ymax = max(bat_box_rot[2, ])
    ) +
    ggplot2::lims(x = c(-1, 9), y = c(-5, 5)) +
    ggplot2::labs(
      x = label,
      y = ggplot2::element_blank()
    ) +
    theme_medium() +
    remove_axes()

  return(swing_plot)
}

{
  open_device("swing_late", medium, height = 2, width = 2)
  swing_late <- create_swing_diagram(
    rotation_angle = 20,
    ball_loc = c(6, -1.1),
    label = "LATE Swing"
  )
  print(swing_late)
  dev.off()
}

{
  open_device("swing_early", medium, height = 2, width = 2)
  swing_early <- create_swing_diagram(
    rotation_angle = -20,
    ball_loc = c(6, 3.3),
    label = "EARLY Swing"
  )
  print(swing_early)
  dev.off()
}


# Approach ----

approach_interpreted <- intention_model$approach |>
    # Add in the fixed effect for strikes
    dplyr::mutate(
      # Convert swing length to inches
      strikes_swing_length = 12 * (strikes_swing_length +
        lme4::fixef(intention_model$fit_swing_length)["strikes"]
      ),
      strikes_bat_speed = strikes_bat_speed +
        lme4::fixef(intention_model$fit_bat_speed)["strikes"]
    ) 

{
  open_device("approach", medium, height = 4, width = 5)
  approach_plot <- approach_interpreted |>
    ggplot2::ggplot(ggplot2::aes(x = strikes_swing_length, y = strikes_bat_speed)) +
    ggplot2::geom_point(color = color_fg, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = color_fg, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = color_fg, linetype = "dashed") +
    ggplot2::lims(x = c(-3, 0.05), y = c(-2.5, 0)) +
    ggplot2::labs(
      x = "Swing Length Reduction per Strike (inches)",
      y = "Bat Speed Reduction per Strike (mph)"
    ) +
    theme_medium()
  print(approach_plot)
  dev.off()
}


# Adaptation ----

{
  open_device("adaptation", medium, height = 3.5, width = 6)

  batter_side_id_1 <- "663538R"
  batter_name_1 <- "Nico Hoerner"

  batter_side_id_2 <- "665742L"
  batter_name_2 <- "Juan Soto"

  plate_loc_grid <- expand.grid(
    plate_x_ref = seq(from = -1.5, to = 1.5, length = 100),
    plate_z = seq(from = 1, to = 4, length = 100)
  ) |>
    dplyr::mutate(balls = 0, strikes = 0)

  plot_data <- plate_loc_grid |>
    dplyr::mutate(
      pred_1 = predict(
        intention_model$fit_swing_length,
        newdata = plate_loc_grid |>
          dplyr::mutate(batter_side_id = batter_side_id_1)
      ),
      pred_2 = predict(
        intention_model$fit_swing_length,
        newdata = plate_loc_grid |>
          dplyr::mutate(batter_side_id = batter_side_id_2)
      )
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
      color = color_bg,
      alpha = 0.5,
      fill = NA,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = strike_zone,
      mapping = ggplot2::aes(x = bat_side_x, xend = bat_side_x, y = 1.1, yend = 3.9),
      inherit.aes = FALSE,
      color = color_bg,
      linewidth = 2
    ) +
    ggplot2::labs(
      subtitle = "Swing length predicted by intention model, assuming 0-0 count",
      fill = "Intended\nSwing\nLength",
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank()
    ) +
    theme_medium() +
    remove_axes() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 12)
    )
  print(adaptation_plot)
  dev.off()
}


# Timing ----

{
  open_device("swing_metrics_intended", medium, height = 3, width = 3)
  swing_metrics_intended_plot <- intention_model$fitted_values |>
    ggplot2::ggplot(ggplot2::aes(x = swing_length, y = bat_speed)) +
    ggplot2::geom_hex(bins = 50) +
    ggplot2::geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam", color = color_fg) +
    ggplot2::scale_fill_gradient(low = color_bg, high = color_blue) +
    ggplot2::labs(
      title = "REAL variation",
      x = "INTENDED Swing Length (feet)",
      y = "INTENDED Bat Speed (mph)",
      fill = "Frequency"
    ) +
    theme_medium() +
    ggplot2::theme(
      legend.position = "none"
    )
  print(swing_metrics_intended_plot)
  dev.off()
}

{
  open_device("swing_metrics_residual", medium, height = 3, width = 3)
  swing_metrics_residual_plot <- intention_model$residuals |>
    ggplot2::ggplot(ggplot2::aes(x = swing_length, y = bat_speed)) +
    ggplot2::geom_hex(bins = 50) +
    ggplot2::geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam", color = color_fg) +
    ggplot2::scale_fill_gradient(low = color_bg, high = color_blue) +
    ggplot2::labs(
      title = "ARITIFACTUAL variation",
      x = "RESIDUAL Swing Length (feet)",
      y = "RESIDUAL Bat Speed (mph)"
    ) +
    theme_medium() +
    ggplot2::theme(
      legend.position = "none"
    )
  print(swing_metrics_residual_plot)
  dev.off()
}


# Effect of approach ----

data_with_approach <- data |>
  dplyr::filter(strikes == 2) |>
  dplyr::mutate(batter_side_id = paste0(batter_id, bat_side)) |>
  dplyr::left_join(intention_model$approach, by = "batter_side_id") |>
  dplyr::mutate(
    approach_bat_speed = strikes * strikes_bat_speed,
    approach_swing_length = strikes * strikes_swing_length
  )

data_with_approach_adjusted <- data_with_approach |>
  dplyr::mutate(
    prob_contact_adj = predict(
      object = approach_model$fit_contact,
      newdata = data_with_approach,
      type = "response"
    ),
    pred_hit_adj = predict(
      object = approach_model$fit_hit,
      newdata = data_with_approach,
      type = "response"
    )
  )

approach_effect_summary <- data_with_approach_adjusted |>
  dplyr::group_by(batter_side_id) |>
  dplyr::summarize(
    strikes_swing_length = mean(strikes_swing_length) +
      lme4::fixef(intention_model$fit_swing_length)["strikes"],
    strikes_bat_speed = mean(strikes_bat_speed) +
      lme4::fixef(intention_model$fit_bat_speed)["strikes"],
    prob_contact_diff = mean(prob_contact_adj - prob_contact),
    pred_hit_diff = mean(pred_hit_adj - pred_hit),
    .groups = "drop"
  ) |>
  # Perform re-centering to compensate for model drift
  dplyr::mutate(
    prob_contact_diff = prob_contact_diff - mean(prob_contact_diff, na.rm = TRUE),
    pred_hit_diff = pred_hit_diff - mean(pred_hit_diff, na.rm = TRUE)
  )

height <- 2
width <- 3

{
  open_device("swing_length_contact", medium = "beamer", height = height, width = width)
  swing_length_contact_plot <- approach_effect_summary |>
    ggplot2::ggplot(ggplot2::aes(x = 24 * strikes_swing_length, y = prob_contact_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg) +
    ggplot2::geom_point(color = color_fg, alpha = 0.5) +
    ggplot2::labs(
      x = "",
      y = "Contact Effect"
    ) +
    ggplot2::scale_y_continuous(breaks = c(-.04, 0, .04), labels = c("-4%", "0%", "+4%")) +
    theme_medium()
  print(swing_length_contact_plot)
  dev.off()
}

{
  open_device("bat_speed_contact", medium = "beamer", height = height, width = width)
  bat_speed_contact_plot <- approach_effect_summary |>
    ggplot2::ggplot(ggplot2::aes(x = 2 * strikes_bat_speed, y = prob_contact_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg) +
    ggplot2::geom_point(col = color_fg, alpha = 0.5) +
    ggplot2::labs(
      x = "",
      y = ""
    ) +
    ggplot2::scale_y_continuous(breaks = c(-.04, 0, .04), labels = c("-4%", "0%", "+4%")) +
    theme_medium()
  print(bat_speed_contact_plot)
  dev.off()
}

{
  open_device("swing_length_power", medium = "beamer", height = height, width = width)
  swing_length_contact_plot <- approach_effect_summary |>
    # 1.239 is wOBA scaling factor
    ggplot2::ggplot(ggplot2::aes(x = 24 * strikes_swing_length, y = 1.239 * pred_hit_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg) +
    ggplot2::geom_point(col = color_fg, alpha = 0.5) +
    ggplot2::labs(
      x = "2-Strike Swing Length Delta (in)",
      y = "xwOBA Effect"
    ) +
    ggplot2::scale_y_continuous(breaks = c(-.04, 0, .04), labels = c("-.040", ".000", "+.040")) +
    theme_medium()
  print(swing_length_contact_plot)
  dev.off()
}

{
  open_device("bat_speed_power", medium = "beamer", height = height, width = width)
  bat_speed_contact_plot <- approach_effect_summary |>
    # 1.239 is wOBA scaling factor
    ggplot2::ggplot(ggplot2::aes(x = 2 * strikes_bat_speed, y = 1.239 * pred_hit_diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = color_fg) +
    ggplot2::geom_point(col = color_fg, alpha = 0.5) +
    ggplot2::labs(
      x = "2-Strike Bat Speed Delta (mph)",
      y = ""
    ) +
    ggplot2::scale_y_continuous(breaks = c(-.04, 0, .04), labels = c("-.040", ".000", "+.040")) +
    theme_medium()
  print(bat_speed_contact_plot)
  dev.off()
}




# Caculate the run value of different approaches ----

# Identify the approaches for which we want to calculate value
approach_matrix <- intention_model$approach |>
  dplyr::select(strikes_swing_length, strikes_bat_speed) |>
  as.matrix()
approach_cluster <- kmeans(approach_matrix, centers = 50)$centers
approach_chull <- approach_matrix[chull(approach_matrix), ]
approach_tibble <- tibble::as_tibble(rbind(c(0, 0), approach_cluster, approach_chull))
approach_list <- split(approach_tibble, f = 1:nrow(approach_tibble))

approach_value_list <- pbapply::pblapply(
  X = approach_list,
  FUN = evaluate_approach,
  pred_outcome_pitch = data,
  approach_model = approach_model,
  linear_weight = linear_weight
)
approach_value <- do.call(dplyr::bind_rows, approach_value_list) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # We're going to say that a zero approach is definitionally zero runs above average
    runs = 500 * (runs - runs[1]),                        # 500 PA scale
    # Make approach relative to no adjustment, instead of being relative to average
    strikes_swing_length = 12 * (strikes_swing_length +   # swing length in inches
      lme4::fixef(intention_model$fit_swing_length)["strikes"]
    ),
    strikes_bat_speed = strikes_bat_speed +
      lme4::fixef(intention_model$fit_bat_speed)["strikes"]
  )

approach_runs_model <- mgcv::gam(
  runs ~ s(strikes_bat_speed, strikes_swing_length),
  data = approach_value
)

approach_grid <- intention_model$approach |>
  with(
    expand.grid(
      strikes_swing_length = seq(-3, 0, length = 200),
      strikes_bat_speed = seq(-2.5, -0.25, length = 200)
    )
  )

approach_grid_with_pred <- approach_grid |>
  dplyr::mutate(runs = predict(approach_runs_model, newdata = approach_grid)) |>
  # Avoid showing predictions that are extrapolating to far
  dplyr::filter(runs < 3, runs > -4.5)

batter <- data |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    batter_name_comma = stringr::str_locate(batter_name, ",")[, "start"],
    batter_name_last = substring(batter_name, 1, batter_name_comma - 1),
    batter_initial_first = substring(batter_name, batter_name_comma + 2, batter_name_comma + 2),
    batter_name_short = paste(batter_initial_first, batter_name_last)
  ) |>
  dplyr::distinct(batter_side_id, batter_name_short)

{
  open_device("approach_run_value", medium, height = 4, width = 5)
  approach_run_value_plot <- approach_grid_with_pred |>
    ggplot2::ggplot(ggplot2::aes(strikes_swing_length, strikes_bat_speed)) +
    ggplot2::geom_raster(ggplot2::aes(fill = runs)) +
    ggplot2::geom_point(data = approach_interpreted, color = "#002B37", alpha = 0.5) +
    ggplot2::scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
    ggplot2::geom_hline(yintercept = 0, color = color_fg, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = color_fg, linetype = "dashed") +
    ggplot2::lims(x = c(-3, 0.05), y = c(-2.5, 0)) +
    ggplot2::labs(
      title = switch(medium,
        beamer = ggplot2::element_blank(),
        article = "Is it good to modulate your swing by count?"
      ),
      subtitle = "Estimated run value of approach applied to average batter",
      x = "Swing Length Reduction per Strike (inches)",
      y = "Bat Speed Reduction per Strike (mph)",
      fill = "Runs per 500 PA"
    ) +
    theme_medium() +
    ggplot2::theme(
      legend.position = c(0.18, 0.71)
    )
  print(approach_run_value_plot)
  dev.off()
}

{
  open_device("approach_run_value_labelled", medium, height = 4, width = 5)
  approach_run_value_labelled_plot <- approach_run_value_plot +
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(label = batter_name_short),
      data = approach_interpreted |>
        dplyr::left_join(batter, by = "batter_side_id") |>
        dplyr::filter(
          batter_side_id %in% c(
            "677594R",  # Julio Rodriguez
            "663538R",  # Nico Hoerner
            "519203L",  # Anthony Rizzo
            "676801R",  # Chas McCormick
            "668939L",  # Adley Rutschman
            "650490R",  # Yandy Diaz
            "665742L"   # Juan Soto
          )
        ),
      color = color_fg,
      fill = color_bg
    )
  print(approach_run_value_labelled_plot)
  dev.off()
}
