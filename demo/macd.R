
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
macd <- function(x, s = 12, l = 26, k = 9) {as.list(as.data.frame(TTR::MACD(x, s, l, k)))}

# macd(dt$close)
dt[, c("macd", "macd_signal") := macd(close, s = 12, l = 26, k = 9)]
dt[, macd_diff := macd - macd_signal]

na.omit(dt) |>
    ggplot2::ggplot(ggplot2::aes(x = datetime)) +
    ddplot::stat_macd(ggplot2::aes(macd = macd, macd_signal = macd_signal, macd_diff = macd_diff)) +
    ## ------------------------------------
    # provide the colnames to the calculated indicators as aes values
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

