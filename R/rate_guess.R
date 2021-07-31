#' Rate a guess, given a set of available combinations
#'
#' @param guess numeric vector with a particular guess
#' @param all_combos matrix of all possible combinations, eg, colors^pegs
#' @param remaining_combos matrix of all remaining available combinations
#'
#' @return integer. Maximum number of combinations we
#' can end up with after making this guess.
#' @export
#'
#' @examples
#' co <- combos(2, 2)
#' rate_guess(all_combos = co, remaining_combos = co, guess = c(1, 1))
rate_guess <- function(all_combos, remaining_combos, guess) {
  possible_responses <- apply(remaining_combos, 1, response, guess)
  return(possible_responses)
}
