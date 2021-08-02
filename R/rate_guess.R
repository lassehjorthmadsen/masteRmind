#' Rate a guess, given a set of available combinations
#'
#' @param guess numeric vector with a particular guess
#' @param remaining_combos matrix of all remaining available combinations
#' @param FUN function used to summarise the number of remaining combinations
#'     after a guess followed by a response
#' @param ... parameters to be passed on to FUN if any
#'
#' @return Summary of the number of combinations we can end up with
#'     after making this guess. Default is max(). So, after guessing
#'     at a code like 1234, any response will leave us with at
#'     \emph{most} this number of remaining possible combinations.
#' @export
#'
#' @examples
#' rate_guess(remaining_combos = combos(2, 2), guess = c(1, 1))
rate_guess <- function(remaining_combos, guess, FUN = max, ...) {

  # Get all possible responses (no of blacks and whites) from guess
  all_responses <- apply(remaining_combos, 1, response, guess) %>% t()

  # We only need to consider *unique* responses
  all_responses <- unique(all_responses)

  # Convert to list of vectors
  possible_responses <- lapply(seq_len(nrow(all_responses)), function(i) all_responses[i, ])

  # Find out how many combos are left after each possible response to guess
  n_combos <- possible_responses %>%
    purrr::map_int(~ reduce_combos(remaining_combos, guess, .x) %>% nrow())

  # Summarise: Default is to return max number of remaining combos after guess
  return(FUN(n_combos, ...))
}
