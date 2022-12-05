
## ddplot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/dereckdemezquita/kucoin.svg?branch=master)](https://travis-ci.org/dereckdemezquita/kucoin)
<!-- badges: end -->

Dereck’s library for plotting `financial` and `time series` data as well
helper functions for plotting in the style of Dereck.

## Installation

You can install the development version of `ddplot` using:

``` r
# install.packages('devtools')
# install.packages("remotes")
remotes::install_github("dereckdemezquita/ddplot")
```

## Financial data

You must provide a function which calculates these indicators and
returns a list which can then be plotted.

### Get financial data

Shown here `kucoin` is a private package by myself for interacting with
the `KuCoin` api. You can use any source of financial data as long as
you pass the variables to the stat correctly.

``` r
ticker <- "BTC/USDT"

dt <- kucoin::get_market_data(
    symbols = ticker,
    from = lubridate::now() - lubridate::days(7),
    to = lubridate::now(),
    frequency = "1 hour"
)

dt
#>        symbol            datetime    open    high     low   close    volume
#>        <char>              <POSc>   <num>   <num>   <num>   <num>     <num>
#>   1: BTC/USDT 2022-11-28 20:00:00 16220.9 16268.0 16213.7 16261.0 169.25765
#>   2: BTC/USDT 2022-11-28 21:00:00 16259.4 16259.4 16189.8 16200.6 263.88177
#>   3: BTC/USDT 2022-11-28 22:00:00 16200.4 16231.9 16198.4 16218.6 121.33248
#>  ---                                                                       
#> 167: BTC/USDT 2022-12-05 18:00:00 17057.2 17127.0 17035.5 17046.7 215.54308
#> 168: BTC/USDT 2022-12-05 19:00:00 17046.7 17058.3 16875.9 16925.3 560.53075
#> 169: BTC/USDT 2022-12-05 20:00:00 16925.3 16946.9 16903.4 16921.6  81.61877
#> 1 variable not shown: [turnover <num>]
```

### Plot financial data

Here I demonstrate how to use the stats for plotting financial data
along with the theme functions included in this package:

1.  `ddplot::stat_bollingerbands()`
2.  `ddplot::stat_candlesticks()`
3.  `ddplot::stat_movingaverages()`

And the theme functions for styling:

1.  `ddplot::theme_dereck_dark()`
2.  `ddplot::theme_dereck_light()`

``` r
## ------
# redifine our function to return a list
ema <- function(x, n, wilder = TRUE) {
    return(as.list(as.data.frame(TTR::EMA(x, n = n, wilder = wilder))))
}

bb <- function(close, n = 2, sd = 2) {
    return(as.list(as.data.frame(TTR::BBands(close, n = n, sd = sd))))
}

## ------
# plot
p <- dt |>
    ggplot2::ggplot(ggplot2::aes(
        x = datetime,
        open = open,
        close = close,
        high = high,
        low = low,
        group = symbol
    )) +
    ## ------------------------------------
    ddplot::stat_candlestick() +
    ## ------------------------------------
    # moving averages
    ddplot::stat_movingaverages(ggplot2::aes(y = close), FUN = ema) +
    ddplot::stat_bollingerbands(ggplot2::aes(y = close), FUN = bb) +
    ## ------------------------------------
    ggplot2::scale_x_continuous(n.breaks = 25, labels = \(x) {
        lubridate::floor_date(lubridate::as_datetime(x), "hours")
    }) +
    ggplot2::scale_y_continuous(n.breaks = 25) +
    ggplot2::labs(
        title = ticker,
        x = "Date",
        y = "Price (USD)"
    ) +
    ddplot::theme_dereck_dark() +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.925, hjust = 0.975),
        panel.grid.minor = ggplot2::element_blank()
    )

print(p)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Now let’s do the same plot in a light theme:

``` r
p + ddplot::theme_dereck_light() +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.925, hjust = 0.975),
        panel.grid.minor = ggplot2::element_blank()
    )
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## Gallery

<p align="center">
<img src="./.graphics/countries-inequality-line-1.jpeg" width="350">
<img src="./.graphics/countries-1-fhos-1.jpeg" width="350">
<img src="./.graphics/gdp-per-capita-1.jpeg" width="350">
<img src="./.graphics/gdp-per-capita-2.jpeg" width="350">
<img src="./.graphics/gdp-per-country-1.jpeg" width="350">
<img src="./.graphics/pop-to-internet-users.jpeg" width="350">
</p>

<img src="./.graphics/countries-u5pop-prc-1.jpeg" width="100%">
