# PURPOSE: Set-up the approach input for Scott's downstream analysis, since the
#          rest of the model structure will be fairly massive

library(tidyverse)
library(brms)
library(tidybayes)
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


# Load the models ---------------------------------------------------------

intent_swing_length_brms <- 
  read_rds("sandbox/ryurko/models/intent_swing_length_full.rds")
intent_bat_speed_brms <- 
  read_rds("sandbox/ryurko/models/intent_bat_speed_full.rds")


# Generate the basic summary tables ---------------------------------------

summary(intent_bat_speed_brms)

summary(intent_swing_length_brms)

# Create the approach table -----------------------------------------------

# Get the posterior means for the random effects in each model:
bat_speed_re <- intent_bat_speed_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  # already grouped by batter_side_id and term:
  summarize(value = mean(r_batter_side_id),
            .groups = "drop") |>
  # Make a wide dataset with a column for each:
  pivot_wider(names_from = term,
              names_glue = "{term}_bat_speed",
              values_from = value)

# Repeat for swing length
swing_length_re <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  summarize(value = mean(r_batter_side_id),
            .groups = "drop") |>
  pivot_wider(names_from = term,
              names_glue = "{term}_swing_length",
              values_from = value)

# Create and save the approach table:
approach <- bat_speed_re |>
  dplyr::select(batter_side_id, strikes_bat_speed) |>
  inner_join(dplyr::select(swing_length_re,
                           batter_side_id, strikes_swing_length),
             by = "batter_side_id")
write_csv(approach, "sandbox/ryurko/models/results/approach.csv")


# Create various random effects plots -------------------------------------

# These are starting points for what we could include

# Get a table of the batters
batter_table <- full_swing_data |>
  group_by(batter_side_id, batter_id, batter_name) |>
  summarize(n_swings = n(),
            n_squared_up = sum(as.numeric(squared_up), na.rm = TRUE),
            .groups = "drop") |>
  mutate(squared_up_rate = n_squared_up / n_swings)

## Bat speed model - mean effects
bat_speed_intent_summary <- intent_bat_speed_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  summarize(speed_mean = mean(r_batter_side_id), 
            .groups = "drop")

## Swing length model - mean effects
swing_length_intent_summary <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id[batter_side_id, term]) |>
  summarize(length_mean = mean(r_batter_side_id), 
            .groups = "drop")

# Combine to create visualizations:
mean_intent_summary <- bat_speed_intent_summary |>
  inner_join(swing_length_intent_summary, 
             by = c("batter_side_id", "term")) |>
  left_join(batter_table, by = "batter_side_id")


# Helper function for rendering math in the facet labels
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


# Visuals for each term - I think these are for the supplement though
{
  sputil::open_device(paste0("figures/intent_re", fig_suffix), height = 6, width = 6)
  intent_re_plot <- mean_intent_summary |>
    filter(n_squared_up >= 25) |>
    mutate(term = 
             case_when(
               term == "Intercept" ~ "gamma_b",
               term == "plate_x_ref" ~ "gamma_b^X",
               term == "plate_z" ~ "gamma_b^Z",
               term == "strikes" ~ "gamma_b^S",
               .default = term
             )) |>
    ggplot(aes(x = speed_mean, y = length_mean)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    geom_text(aes(label = batter_name), size = 1, color = color_blue,
              alpha = 0.7) +
    facet_wrap(~term, ncol = 2, scales = "free",
               labeller = as_labeller(math_labeller, 
                                      default = label_parsed)) +
    labs(x = "Posterior mean for bat speed intent random effect",
         y = "Posterior mean for swing length intent random effect") +
    sputil::theme_sleek(mode = fig_mode)
  print(intent_re_plot)
  dev.off()
}

# Make visual for alpha terms - this should go in the main text I think
# given the variance estimates we have here, and since it is unique

## Bat speed model - alpha effects
bat_speed_alpha_intent_summary <- intent_bat_speed_brms |>
  spread_draws(r_batter_side_id__alpha[batter_side_id, term]) |>
  summarize(speed_mean = mean(r_batter_side_id__alpha), 
            .groups = "drop")

## Swing length model - mean effects
swing_length_alpha_intent_summary <- intent_swing_length_brms |>
  spread_draws(r_batter_side_id__alpha[batter_side_id, term]) |>
  summarize(length_mean = mean(r_batter_side_id__alpha), 
            .groups = "drop")

# Combine to create visualizations:
alpha_intent_summary <- bat_speed_alpha_intent_summary |>
  inner_join(swing_length_alpha_intent_summary, 
             by = c("batter_side_id", "term")) |>
  left_join(batter_table, by = "batter_side_id")

# Visuals for each term (could facet, that would be intense though)
{
  sputil::open_device(paste0("figures/alpha_re", fig_suffix), height = 5, width = 7)
  alpha_intent_plot <- alpha_intent_summary |>
    filter(n_squared_up >= 25) |>
    ggplot(aes(x = speed_mean, y = length_mean)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = color_fg, alpha = 0.5) +
    geom_text(aes(label = batter_name), size = 2, color = color_blue, alpha = 0.7) +
    labs(x = latex2exp::TeX("Posterior mean for bat speed alpha random effect"),
         y = "Posterior mean for swing length alpha random effect") +
    sputil::theme_sleek(mode = fig_mode)
  print(alpha_intent_plot)
  dev.off()
  }


# Visualize estimated intention distributions for different players -------

# Looking at length, highest positive alpha random intercept
batter_side_id_1 <- "592206R"
batter_name_1 <- "Nick Castellanos"

# Lowest
batter_side_id_2 <- "543760R"
batter_name_2 <- "Marcus Semien"

# Reasonable number of swings and near average
batter_side_id_3 <- "596019L"
batter_name_3 <- "Francisco Lindor"

# Set-up the grid for getting the skew normal predictions
pitch_pred_grid <- tibble(plate_x_ref = 0, plate_z = 2.5, 
                          pitcher_id = 682120, balls = 0, strikes = 0)

# Define a helper function that returns the posterior means for the SK
# distribution parameters, and will use those to draw the densities:
get_posterior_mean_sk <- function(batter_id, batter_name, intent_model,
                                  pred_grid = pitch_pred_grid) {
  
  map_dfc(c("mu", "sigma", "alpha"),
          function(param) {
            
            param_post_mean <- 
              tibble(post_val = mean(
                posterior_epred(intent_model, dpar = param,
                                newdata = pred_grid |>
                                  mutate(batter_side_id = batter_id))[,1]))
            colnames(param_post_mean) <- param
            param_post_mean
          }) |>
    mutate(batter_side_id = batter_id, 
           batter_name = batter_name)
  
}

batter_sk_params <- bind_rows(get_posterior_mean_sk(batter_side_id_1,
                                                    batter_name_1,
                                                    intent_swing_length_brms),
                              get_posterior_mean_sk(batter_side_id_2,
                                                    batter_name_2,
                                                    intent_swing_length_brms),
                              get_posterior_mean_sk(batter_side_id_3,
                                                    batter_name_3,
                                                    intent_swing_length_brms))


x_lower_val <- 5
x_upper_val <- 9
ggplot(data.frame(x = c(x_lower_val , x_upper_val)), 
       aes(x = x)) + 
  xlim(c(x_lower_val , x_upper_val)) + 
  stat_function(fun = dskew_normal, 
                args = list(mu = 7.4,
                            sigma = batter_sk_params$sigma[1],
                            alpha = batter_sk_params$alpha[1]), 
                aes(colour = "Nick Castellanos")) + 
  stat_function(fun = dskew_normal, 
                args = list(mu = 7.4,
                            sigma = batter_sk_params$sigma[2],
                            alpha = batter_sk_params$alpha[2]), 
                aes(colour = "Marcus Semien")) + 
  stat_function(fun = dskew_normal, 
                args = list(mu = 7.4,
                            sigma = batter_sk_params$sigma[3],
                            alpha = batter_sk_params$alpha[3]), 
                aes(colour = "Francisco Lindor")) + 
  scale_color_manual("Batter", 
                     # Colorblind palette
                     values = c("#000000", "#E69F00", "#56B4E9")) +
  labs(x = "Swing length", y = "Density") + 
  sputil::theme_sleek(mode = fig_mode)
# Ugh - that's not very revealing... going to ditch that figure

