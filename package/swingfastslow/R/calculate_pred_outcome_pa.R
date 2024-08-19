#' Calculate predicted plate appearance outcomes from predicted pitch outcomes
#' 
#' In this function, we take a marginalized prediction across pitches within each count to get the
#' distribution of outcomes in each count and calculate the expected outcome of a plate appearance
#' assuming a Markov model from count to count. The input needs to have one row for each count,
#' but counts can be repeated if the the input table is grouped and each count appears exactly once
#' within each group (allowing you to simultaneously calculate the run value for plate appearances
#' under mutiple different assumptions).
#' 
#' @param pred_outcome_by_count a table with columns `balls`, `strikes`, `mean_prob_hbp`,
#'   `mean_prob_ball`, `mean_prob_strike`, `mean_prob_foul`, `mean_prob_fair` and `mean_pred_hit`
#' @param linear_weight a table with columns `event` and `linear_weight`
#' @param nonterminal_prob_threshold this numeric threshold determines how long we run the Markov
#'   chain to calculate the terminal state probabilities from each starting state. We increase the
#'   number of steps in the Markov chain until the probabilities for non-terminal states are below
#'   `nonterminal_prob_threshold` (default 0.0001%).
#' 
#' @returns a table with column `linear_weight` reflecting the run value of a plate apperance with
#'   the specificed pitch outcome distribution by count
#' 
#' @export
#' 
calculate_pred_outcome_pa <- function(pred_outcome_by_count,
                                      linear_weight,
                                      nonterminal_prob_threshold = 1e-6) {

  # We want to respect the grouping variables specified prior to this function.
  # This allows users to calculate the run value for multiple different assumptions.
  existing_group_vars <- dplyr::group_vars(pred_outcome_by_count)
  group_vars_with_state <- c(
    existing_group_vars,
    "pre_balls", "pre_strikes", "pre_state", "post_balls", "post_strikes", "post_state"
  )
  group_vars_with_event <- c(existing_group_vars, "balls", "strikes", "event")

  pred_hit_by_count <- pred_outcome_by_count |>
    dplyr::select(dplyr::all_of(c(existing_group_vars, "balls", "strikes", "mean_pred_hit")))

  prob_outcome_by_count <- pred_outcome_by_count |>
    tidyr::pivot_longer(dplyr::starts_with("mean_"), values_to = "prob") |>
    dplyr::filter(substring(name, 6, 9) == "prob") |>
    dplyr::transmute(pre_balls = balls, pre_strikes = strikes, outcome = substring(name, 11), prob)

  outcome_transition_by_count <- expand.grid(
    pre_balls = 0:3,
    pre_strikes = 0:2,
    pre_state = "Nonterminal",
    outcome = unique(prob_outcome_by_count$outcome)
  ) |>
    dplyr::mutate(
      post_balls = pre_balls + (outcome == "ball"),
      post_strikes = pre_strikes + (outcome == "strike") + (pre_strikes < 2 & outcome == "foul"),
      post_state = dplyr::case_when(
        outcome == "hbp" ~ "Hit By Pitch",
        outcome == "fair" ~ "Fair Ball",
        post_balls == 4 ~ "Walk",
        post_strikes == 3 ~ "Strikeout",
        TRUE ~ "Nonterminal"
      )
    )

  prob_transition_by_count <- prob_outcome_by_count |>
    dplyr::left_join(outcome_transition_by_count, by = c("pre_balls", "pre_strikes", "outcome")) |>
    dplyr::group_by_at(group_vars_with_state) |>
    dplyr::summarize(prob = sum(prob), .groups = "drop")
  
  prob_terminal_by_count <- prob_transition_by_count

  while(
    with(prob_terminal_by_count, max(prob[post_state == "Nonterminal"])) > nonterminal_prob_threshold
  ) {
    prob_terminal_by_count <- prob_terminal_by_count |>
      dplyr::left_join(
        y = prob_transition_by_count,
        by = c(
          existing_group_vars,
          "post_balls" = "pre_balls", "post_strikes" = "pre_strikes", "post_state" = "pre_state"
        ),
        suffix = c("_1", "_2"),
        relationship = "many-to-many"
      ) |>
      # If post_*_2 is NA, that's because post_* is a terminal state.
      # We want to stay in that terminal state with probability 1.
      dplyr::mutate(
        post_balls = dplyr::coalesce(post_balls_2, post_balls),
        post_strikes = dplyr::coalesce(post_strikes_2, post_strikes),
        post_state = dplyr::coalesce(post_state_2, post_state),
        prob_2 = dplyr::coalesce(prob_2, 1)
      ) |>
      dplyr::group_by_at(group_vars_with_state) |>
      dplyr::summarize(prob = sum(prob_1 * prob_2), .groups = "drop") |>
      dplyr::select(
        dplyr::all_of(existing_group_vars),
        dplyr::starts_with("pre_"),
        dplyr::starts_with("post_"),
        prob
      )
  }

  pred_outcome_pa <- prob_terminal_by_count |>
    dplyr::filter(pre_balls == 0, pre_strikes == 0, post_state != "Nonterminal") |>
    dplyr::mutate(
      event = post_state,
      balls = ifelse(post_state == "Fair Ball", post_balls, NA),
      strikes = ifelse(post_state == "Fair Ball", post_strikes, NA)
    ) |>
    dplyr::group_by_at(group_vars_with_event) |>
    dplyr::summarize(prob = sum(prob), .groups = "drop") |>
    dplyr::left_join(pred_hit_by_count, by = c(existing_group_vars, "balls", "strikes")) |>
    dplyr::left_join(linear_weight, by = "event") |>
    dplyr::group_by_at(existing_group_vars) |>
    dplyr::summarize(
      runs = weighted.mean(dplyr::coalesce(mean_pred_hit, linear_weight), w = prob)
    )
  
  return(pred_outcome_pa)
}
