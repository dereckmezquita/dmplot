
# get some financial data
# kucoin is private package - you can use any data source
ticker <- "BTC/USDT"

dt <- kucoin::get_market_data(
    symbols = ticker,
    from = "2022-11-28 15:29:43 EST", # lubridate::now() - lubridate::days(7),
    to = "2022-12-05 15:29:31 EST",# lubridate::now(),
    frequency = "1 hour"
)

dt

# we need a function that calculates the indicator for us
# typically I like to write my own functions in C++; in this case we will use TTR's
# the stat expects a named list to be returned - we redefine ttr
bb <- function(close, n = 10, sd = 2) {
    return(as.list(as.data.frame(TTR::BBands(close, n = n, sd = sd))))
}

# calculate the bolinger bands
dt[, c("bb_lower", "bb_mavg", "bb_upper", "bb_pct") := bb(close, n = 10, sd = 2)]

dt |>
    ggplot2::ggplot(ggplot2::aes(
        x = datetime,
        open = open,
        close = close,
        high = high,
        low = low,
        group = symbol
    )) +
    ## ------------------------------------
    dmplot::stat_candlestick() +
    ## ------------------------------------
    # provide the colnames to the calculated indicators as aes values
    dmplot::stat_bollingerbands(ggplot2::aes(ymin = bb_lower, mavg = bb_mavg, ymax = bb_upper), colour = list("pink", "cyan", "cyan")) +
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
    dmplot::theme_dereck_dark() +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.925, hjust = 0.975),
        panel.grid.minor = ggplot2::element_blank()
    )
