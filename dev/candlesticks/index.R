dt <- data.table::fread("./demo/data/bitcoin-hourly-data.csv")

lines <- data.table::data.table(
    x = dt$datetime,
    y = dt$close,
    group = 1
)

lines <- rbind(lines, data.table::data.table(
    x = dt$datetime,
    y = dt$high,
    group = 2
))

dt |>
    ggplot2::ggplot() +
    stat_candlestick(ggplot2::aes(
        x = datetime,
        open = open,
        close = close,
        high = high,
        low = low,
        group = symbol
    )) +
    ggplot2::geom_line(data = lines, ggplot2::aes(
        x = x,
        y = y,
        group = group,
        colour = as.character(group)
    ))
