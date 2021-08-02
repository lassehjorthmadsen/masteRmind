#' Makes a guess a the secret code
#'
#' @param all_combos tibble with all combinations to pick a guess from
#' @param remaining_combos tibble with available combinations to guess at
#'
#' @return numeric vector representing a valid guess at secret code
#' @export
#'
#' @examples
#' co <- combos(2, 2)
#' make_guess(all_combos = co, remaining_combos = co)
#'
make_guess <- function(all_combos, remaining_combos) {

  # Convert to list of vectors
  possible_guesses <- lapply(seq_len(nrow(all_combos)), function(i) as.matrix(all_combos)[i, ])

  # Rate every possible guess based on every possible response
  ratings <- possible_guesses %>%
    purrr::map_int(~ rate_guess(remaining_combos, .x))

  # Find id of guesses with the lowest guaranteed remaining combos
  best_guesses <- which(ratings == min(ratings))

  return(possible_guesses[[best_guesses[1]]])
}
