options(
    "datatable.print.topn" = 3,
    "datatable.print.nrows" = 50,
    "datatable.print.class" = TRUE,
    "datatable.print.trunc.cols" = TRUE
)

box::use(kucoin)
box::use(dt = data.table)
box::use(ggplot2)
box::use(dmplot)
box::use(./stat_movingaverages[ stat_movingaverages, stat_candlestick ])

ticker <- "BTC/USDT"

data <- kucoin$get_market_data(
    symbols = ticker,
    from = "2024-06-30 11:34:17 EST", # lubridate::now() - lubridate::days(7),
    to = "2024-07-07 11:34:12 EST", # lubridate::now(),
    frequency = "1 hour"
)

head(data)

candle_plot <- data |>
    ggplot2$ggplot(ggplot2$aes(
        x = datetime,
        open = open,
        high = high,
        low = low,
        close = close
    )) +
    ## ------------------------------------
    dmplot$stat_candlestick() +
    ## ------------------------------------
    ggplot2$scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
    ggplot2$labs(
        title = paste(ticker, "- Candlestick with EMA and Bollinger Bands"),
        x = "Date",
        y = "Price (USD)"
    ) +
    dmplot$theme_dereck_dark() +
    ggplot2$theme(axis.text.x = ggplot2$element_text(angle = 45, hjust = 1))

print(candle_plot)

sma <- function(x, n) {
    return(as.list(as.data.frame(TTR::SMA(x, n = n))))
}

data2 <- dt$copy(data)

data2[, sma_short := sma(close, n = 20)[[1]]]
data2[, sma_long := sma(close, n = 50)[[1]]]

head(data2)

sma_layer <- data2 |>
    ggplot2$ggplot(ggplot2$aes(
        x = datetime,
        open = open,
        high = high,
        low = low,
        close = close
    )) +
    stat_movingaverages(
        ggplot2$aes(short = sma_short, long = sma_long),
        alpha = list(mavg = 0.5)
    )

candle_plot + sma_layer

