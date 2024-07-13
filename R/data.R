#' Bitcoin Hourly Data (One Year)
#'
#' A dataset containing hourly Bitcoin price and volume data for a one-year period.
#'
#' @format A data frame with 8756 rows and 8 variables:
#' \describe{
#'   \item{symbol}{Character. Trading symbol, e.g., 'BTC/USDT'}
#'   \item{datetime}{POSIXct. Date and time of the observation}
#'   \item{open}{Numeric. Opening price for the hour}
#'   \item{high}{Numeric. Highest price during the hour}
#'   \item{low}{Numeric. Lowest price during the hour}
#'   \item{close}{Numeric. Closing price for the hour}
#'   \item{volume}{Numeric. Trading volume during the hour}
#'   \item{turnover}{Numeric. Trading turnover (in USD) during the hour}
#' }
#' @details This dataset was created using the `get_market_data()` function, 
#'   fetching data for the BTC/USDT pair over a one-year period with hourly frequency.
#'   The data spans from July 14, 2023 to July 13, 2024.
#' @source Cryptocurrency exchange data, retrieved using `get_market_data()` function
#' @examples
#' data(btc_1_year_hourly)
#' head(btc_1_year_hourly)
#' summary(btc_1_year_hourly$close)
"btc_1_year_hourly"

#' Feature Counts for Genes
#'
#' A dataset containing feature counts for various genes across different samples.
#'
#' @format A data frame with rows and variables:
#' \describe{
#'   \item{GeneID}{Ensembl gene identifier}
#'   \item{GeneSymbol}{Gene symbol}
#'   \item{GeneBiotype}{Type of gene}
#'   \item{B24568, B24560, ...}{Counts for each sample}
#' }
#' @source Gene expression study
"feature_counts"

#' Volcano Differential Expression Data
#'
#' A dataset containing differential expression analysis results for creating volcano plots.
#'
#' @format A data frame with rows and 4 variables:
#' \describe{
#'   \item{feature}{Feature identifier}
#'   \item{log2FC}{Log2 fold change}
#'   \item{p_value}{P-value}
#'   \item{fdr}{False Discovery Rate}
#' }
#' @source Differential expression analysis
"diff_expr_res"
