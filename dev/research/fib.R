box::use(dmplot)
box::use(dmplot[ fib ])
box::use(dt = data.table)

box::use(kucoin[ get_market_data ])

ticker <- "BTC/USDT"

data <- get_market_data(
    symbols = ticker,
    from = lubridate::now() - lubridate::days(100),
    to = lubridate::now(),
    frequency = "1 hour"
)

high_price <- max(data$high)
low_price <- min(data$low)

fib_levels <- dmplot$fib(high_price, low_price)

data.frame(fib_levels) |>
    ggplot2::ggplot(ggplot2::aes(x = levels, y = prices)) +
    ggplot2::geom_point()

box::use(ggplot2)

# Assuming 'data' has columns 'date' and 'close'
ggplot2$ggplot(data, ggplot2$aes(x = datetime, y = close)) +
    ggplot2$geom_line() +
    ggplot2$geom_hline(yintercept = fib_levels$prices, color = "blue", linetype = "dashed") +
    ggplot2$scale_y_continuous(breaks = fib_levels$prices, labels = paste0(fib_levels$levels * 100, "%")) +
    ggplot2$labs(
        title = "Price Chart with Fibonacci Retracement Levels",
        x = "Date",
        y = "Price"
    )
