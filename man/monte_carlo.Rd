% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{monte_carlo}
\alias{monte_carlo}
\title{Monte Carlo Simulation in C++}
\usage{
monte_carlo(seed_price, daily_vol, num_sims, num_days)
}
\arguments{
\item{seed_price}{numeric. The starting price for the simulation.}

\item{daily_vol}{numeric. The daily volatility of the price movements.}

\item{num_sims}{integer. The number of simulation paths to generate.}

\item{num_days}{integer. The number of days to simulate for each path.}
}
\value{
A list containing two data frames:
  \item{simulations}{A data frame with columns 'close' (simulated prices) and 'simulation' (simulation index)}
  \item{end_prices}{A data frame with columns 'close' (final prices) and 'simulation' (simulation index)}
}
\description{
This function performs a Monte Carlo simulation for price movements using C++.
}
\details{
This function uses a geometric Brownian motion model to simulate price movements.
For each simulation path, it generates daily returns using a normal distribution
with mean 0 and standard deviation equal to the provided daily volatility.
The function is implemented in C++ for improved performance, especially for
large numbers of simulations or long time horizons.
}
\examples{
\dontrun{
results <- monte_carlo(seed_price = 100, daily_vol = 0.02, num_sims = 1000, num_days = 30)
head(results$simulations)
head(results$end_prices)
}

}
