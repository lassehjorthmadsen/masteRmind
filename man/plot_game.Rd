% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_game.R
\name{plot_game}
\alias{plot_game}
\title{Plot a particular game history}
\usage{
plot_game(guess_history, response_history, color_scheme = NULL)
}
\arguments{
\item{guess_history}{list of numeric vectors representing guesses}

\item{response_history}{list of numeric vectors representing responses}

\item{color_scheme}{charactor with colors used for numbers from 0 to 9}
}
\value{
a ggplot obbject
}
\description{
Plot a particular game history
}
\examples{
plot_game(guesses, responses)

}
