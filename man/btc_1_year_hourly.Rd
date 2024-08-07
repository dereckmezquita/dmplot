% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{btc_1_year_hourly}
\alias{btc_1_year_hourly}
\title{Bitcoin Hourly Data (One Year)}
\format{
A data frame with 8756 rows and 8 variables:
\describe{
  \item{symbol}{Character. Trading symbol, e.g., 'BTC/USDT'}
  \item{datetime}{POSIXct. Date and time of the observation}
  \item{open}{Numeric. Opening price for the hour}
  \item{high}{Numeric. Highest price during the hour}
  \item{low}{Numeric. Lowest price during the hour}
  \item{close}{Numeric. Closing price for the hour}
  \item{volume}{Numeric. Trading volume during the hour}
  \item{turnover}{Numeric. Trading turnover (in USD) during the hour}
}
}
\source{
Cryptocurrency exchange data, retrieved using `get_market_data()` function
}
\usage{
btc_1_year_hourly
}
\description{
A dataset containing hourly Bitcoin price and volume data for a one-year period.
}
\details{
This dataset was created using the `get_market_data()` function, 
  fetching data for the BTC/USDT pair over a one-year period with hourly frequency.
  The data spans from July 14, 2023 to July 13, 2024.
}
\examples{
data(btc_1_year_hourly)
head(btc_1_year_hourly)
summary(btc_1_year_hourly$close)
}
\keyword{datasets}
