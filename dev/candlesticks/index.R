dt <- data.table::fread("./demo/data/bitcoin-hourly-data.csv")

dt |>
    ggplot2::ggplot() +
    stat_candlestick(ggplot2::aes(
        x = datetime,
        open = open,
        close = close,
        high = high,
        low = low,
        group = symbol
    ))