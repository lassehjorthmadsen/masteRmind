#' Find subset of available combinations that are consistent with
#' a given guess and response
#'
#' @param combos tibble with all available combinations
#' @param guess numeric vector with a particular guess
#' @param resp named numeric vector of length 2 with response from that guess
#'
#' @return tibble, subset of the original
#' @export
#'
#' @examples
#' reduce_combos(combos = matrix(c(1,1,2,2,1,2,1,2), 4), guess = c(1, 1), resp = c(1, 0))
#'
reduce_combos <- function(combos, guess, resp) {
  agree <- apply(combos, 1, response, guess) == resp
  ids <- apply(agree, 2, all)
  return(tibble::as_tibble(combos[ids, ]))
}
