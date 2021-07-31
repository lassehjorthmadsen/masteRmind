#' Create matrix with all possible code combinations
#'
#' @param colors integer from 1 to 9. Number of colors in game
#' @param pegs integer from 1 to 9. Number of pegs (slots) in game
#'
#' @return tibble with all possible code combinations
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' all_combos <- combos(colors = 8, pegs = 4)
#'
combos <- function(colors, pegs) {
  matrix(rep(0:(colors - 1), pegs), ncol = pegs,
         dimnames = list(NULL, letters[1:pegs])) %>%
    as.data.frame() %>%
    expand.grid() %>%
    dplyr::tibble()
  }
