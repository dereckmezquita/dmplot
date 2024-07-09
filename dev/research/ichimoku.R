box::use(dmplot)
box::use(ggplot2)
box::use(dt = data.table)
box::use(kucoin[ get_market_data ])

ticker <- "BTC/USDT"

data <- get_market_data(
    symbols = ticker,
    from = lubridate::now() - lubridate::days(100),
    to = lubridate::now(),
    frequency = "1 hour"
)

tenkan_period = 9L
kijun_period = 26L
senkou_period = 52L

dmplot$ichimoku_cloud2(
    data$high, data$low, data$close, tenkan_period, kijun_period, senkou_period
) |> dt$as.data.table()

data

data[, 
    c("tenkan_sen", "kijun_sen", "senkou_span_a", "senkou_span_b", "chikou_span") :=
    dmplot$ichimoku_cloud2(
        high, low, close, tenkan_period, kijun_period, senkou_period
    )
]

summary(data$chikou_span)

# Create the plot
tail(na.omit(data), 250) |>
    ggplot2$ggplot(ggplot2$aes(x = datetime)) +
    ggplot2$geom_line(ggplot2$aes(y = close), color = "black") +
    ggplot2$geom_line(ggplot2$aes(y = tenkan_sen), color = "blue") +
    ggplot2$geom_line(ggplot2$aes(y = kijun_sen), color = "red") +
    ggplot2$geom_ribbon(
        ggplot2$aes(ymin = pmin(senkou_span_a, senkou_span_b),
        ymax = pmax(senkou_span_a, senkou_span_b)),
        fill = "green",
        alpha = 0.2
    ) +
    ggplot2$geom_line(ggplot2$aes(y = chikou_span), color = "purple")
