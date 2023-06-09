% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_mc.R
\name{generate_mc}
\alias{generate_mc}
\title{Generate Monte Carlo simulation}
\usage{
generate_mc(num.iter = 10000, beta0 = -3.45, cores = 1)
}
\arguments{
\item{num.iter}{number of iterations}

\item{beta0}{for treatment effect generation; determines imbalance level}

\item{cores}{the number of cores for parallelization}
}
\value{
Monte Carlo simulation estimates
}
\description{
Generate 10,000 simulated data set, fit survival models and return estimates
}
