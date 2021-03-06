% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response.R
\name{response}
\alias{response}
\title{Generates a response, given a code and a guess}
\usage{
response(code, guess)
}
\arguments{
\item{code}{numeric vector of length 1 to 9}

\item{guess}{numeric vector of length 1 to 9}
}
\value{
named vector of length 2, with names 'black'
and 'white'
}
\description{
Generates a response, given a code and a guess
}
\examples{
response(code = c(1, 2, 3, 4), guess = c(1, 2, 4, 3))

}
