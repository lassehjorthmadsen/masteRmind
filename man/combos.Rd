% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combos.R
\name{combos}
\alias{combos}
\title{Create matrix with all possible code combinations}
\usage{
combos(colors, pegs)
}
\arguments{
\item{colors}{integer from 1 to 9. Number of colors in game}

\item{pegs}{integer from 1 to 9. Number of pegs (slots) in game}
}
\value{
tibble with all possible code combinations
}
\description{
Create matrix with all possible code combinations
}
\examples{
all_combos <- combos(colors = 8, pegs = 4)

}
