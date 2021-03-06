% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enttn.R
\name{enttn}
\alias{enttn}
\title{Truncated Normal Distribution Entropy}
\usage{
enttn(.mean = rep(0, 1), .sd = rep(1, length(.mean)), .low = rep(-Inf,
  length(.mean)), .high = rep(Inf, length(.mean)))
}
\arguments{
\item{.mean}{Length K vector with the means of the K Normal
distributions prior to truncation}

\item{.sd}{Length K vector with the standard deviations of the K Normal
distributions prior to truncation}

\item{.low}{Length K vector with the lower truncation bound of the K
Normal distributions prior to truncation}

\item{.high}{Length K vector with the upper truncation bound of the K
Normal distributions prior to truncation}
}
\value{
Length K vector with the entropies associated with each of the K
Truncated Normal distributions
}
\description{
Calculate entropy of Truncated Normal distributions
}
\examples{
lows <- c(-1, 5, -100, 4, 4, -100, 7)
highs <- c(1, 100, 10, 7, 4.1, 100, 100)
enttn(.mean = rep(0, length(lows)),
      .sd = rep(1, length(lows)),
      .low = lows,
      .high = highs
      )

}
\author{
Jonathan Olmsted
}
