# PURPOSE: Generate the 'adaptation' figure following Scott's code

library(tidyverse)
devtools::load_all("package/swingfastslow")

# Read in the data --------------------------------------------------------

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

# Load the primary fastballs table
primary_fastballs <- read_csv("data/primary_fastballs.csv")

intent_swing_data <- full_swing_data |>
  filter(pitch_type %in% c("FF", "SI", "FC")) |>
  left_join(primary_fastballs, by = "pitcher_id") |>
  mutate(is_primary = pitch_type == primary_fastball) |>
  # only use primary fastballs and squared up swings
  filter(is_primary, squared_up)

# Read in the models ------------------------------------------------------

intent_swing_length_brms <- 
  read_rds("sandbox/ryurko/models/intent_swing_length_full.rds")

# Follow Scott's procedure to get the predictions -------------------------

fig_mode <- "light"
fig_type <- "pdf"
fig_suffix <- glue::glue("{ifelse(fig_mode == 'dark', '_dark', '')}.{fig_type}")
color_fg <- sputil::color("fg", mode = fig_mode)
color_bg <- sputil::color("bg", mode = fig_mode)
color_base2 <- sputil::color("base2", mode = fig_mode)
color_blue <- sputil::color("blue", mode = fig_mode)


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
        intent_swing_length_brms,
        newdata = plate_loc_grid |>
          dplyr::mutate(batter_side_id = batter_side_id_1)
      )[, "Estimate"],
      pred_2 = predict(
        intent_swing_length_brms,
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
  
  strike_zone <- full_swing_data |>
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