# PURPOSE: Visualize example batter distributions to include for motivating
#          the skew-normal fit

library(tidyverse)
devtools::load_all("package/swingfastslow")

# Load the data -----------------------------------------------------------

# This is the full swing dataset, while the intent data was trained on 
# squared up contact against pitchers' primary fastballs
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

primary_fastballs <- read_csv("data/primary_fastballs.csv")

intent_swing_data <- full_swing_data |>
  filter(pitch_type %in% c("FF", "SI", "FC")) |>
  left_join(primary_fastballs, by = "pitcher_id") |>
  mutate(is_primary = pitch_type == primary_fastball) |>
  # only use primary fastballs and squared up swings
  filter(is_primary, squared_up)

# Visualize example player distributions ----------------------------------

math_labeller <- function(string) {
  latex2exp::TeX(paste0("$\\", string, "$"))
}

fig_mode <- "light"
fig_type <- "pdf"
fig_suffix <- glue::glue("{ifelse(fig_mode == 'dark', '_dark', '')}.{fig_type}")
color_fg <- sputil::color("fg", mode = fig_mode)
color_bg <- sputil::color("bg", mode = fig_mode)
color_base2 <- sputil::color("base2", mode = fig_mode)
color_blue <- sputil::color("blue", mode = fig_mode)

{
  sputil::open_device(paste0("figures/ex_speed_intent_distr", fig_suffix), 
                      height = 3, width = 6)
  speed_intent_plot <- intent_swing_data |>
    filter(batter_name %in% c("Cruz, Oneil", "Arraez, Luis", 
                              "Stanton, Giancarlo")) |>
    ggplot(aes(y = bat_speed, x = batter_name)) +
    geom_violin(fill = NA, color = color_blue) +
    coord_flip() +
    labs(x = "Batter",
         y = "Bat Speed (mph)") +
    sputil::theme_sleek(mode = fig_mode)
  print(speed_intent_plot)
  dev.off()
}


