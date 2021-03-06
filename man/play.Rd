% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/play.R
\name{play}
\alias{play}
\title{Play the game of Mastermind with a given configuration}
\usage{
play(colors, pegs, allow_duplicates = TRUE)
}
\arguments{
\item{colors}{integer from 1 to 9. Number of colors in game}

\item{pegs}{integer from 1 to 9. Number of pegs (slots) in game}

\item{allow_duplicates}{boolean. TRUE (default) means duplicate colors allowed in code}
}
\description{
Play the game of Mastermind with a given configuration
}
\examples{
play(colors = 8, pegs = 4)

}
