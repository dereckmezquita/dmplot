# Wrapper functions for C++ code in dmplot package

#' @useDynLib dmplot, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

#' Bollinger Bands
#'
#' @param price A numeric vector of prices
#' @param n The period for the moving average
#' @param sd The number of standard deviations for the bands
#' @return A list containing the lower band, moving average, upper band, and percentage B
#' @export
bb <- function(price, n, sd = 2) {
    .Call('_dmplot_bb', PACKAGE = 'dmplot', price, n, sd)
}

#' Exponential Moving Average
#'
#' @param price A numeric vector of prices
#' @param n The period for the EMA
#' @param wilder Whether to use Wilder's smoothing
#' @return A numeric vector containing the EMA values
#' @export
ema <- function(price, n, wilder = FALSE) {
    .Call('_dmplot_ema', PACKAGE = 'dmplot', price, n, wilder)
}

#' Moving Average Convergence Divergence (MACD)
#'
#' @param price A numeric vector of prices
#' @param s The short-term period
#' @param l The long-term period
#' @param k The signal line period
#' @param percent Whether to return the MACD as a percentage
#' @return A list containing the MACD and signal line values
#' @export
macd <- function(price, s, l, k, percent = TRUE) {
    .Call('_dmplot_macd', PACKAGE = 'dmplot', price, s, l, k, percent)
}

#' Momentum
#'
#' @param price A numeric vector of prices
#' @param n The period for momentum calculation
#' @return A numeric vector containing the momentum values
#' @export
mom <- function(price, n) {
    .Call('_dmplot_mom', PACKAGE = 'dmplot', price, n)
}

#' Monte Carlo Simulation
#'
#' @param seed_price The starting price
#' @param daily_vol The daily volatility
#' @param num_sims The number of simulations to run
#' @param num_days The number of days to simulate
#' @return A list containing the simulation results and end prices
#' @export
monte_carlo <- function(seed_price, daily_vol, num_sims, num_days) {
    .Call('_dmplot_monte_carlo', PACKAGE = 'dmplot', seed_price, daily_vol, num_sims, num_days)
}

#' Rate of Change (ROC)
#'
#' @param price A numeric vector of prices
#' @param n The period for ROC calculation
#' @param type The type of ROC calculation: 'c' for continuous (default) or 'd' for discrete
#' @return A numeric vector containing the ROC values
#' @export
roc <- function(price, n, type = 'c') {
    .Call('_dmplot_roc', PACKAGE = 'dmplot', price, n, type)
}

#' Relative Strength Index (RSI)
#'
#' @param price A numeric vector of prices
#' @param n The period for RSI calculation
#' @param method The method for average calculation: 'e' for EMA (default) or 's' for SMA
#' @return A numeric vector containing the RSI values
#' @export
rsi <- function(price, n, method = 'e') {
    .Call('_dmplot_rsi', PACKAGE = 'dmplot', price, n, method)
}

#' Simple Moving Average (SMA)
#'
#' @param price A numeric vector of prices
#' @param n The period for SMA calculation
#' @return A numeric vector containing the SMA values
#' @export
sma <- function(price, n) {
    .Call('_dmplot_sma', PACKAGE = 'dmplot', price, n)
}