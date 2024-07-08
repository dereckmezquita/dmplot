box::use(dmplot)
box::use(dt = data.table)

data <- dt$fread("./demo/data/bitcoin-hourly-data.csv")

high_price <- max(data$high)
low_price <- min(data$low)

fib_levels <- dmplot$fib(high_price, low_price)

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

## ---------------------------
data2 <- dt$copy(data)

tenkan_period = 9L
kijun_period = 26L
senkou_period = 52L
ichimoku_data <- dmplot$ichimoku_cloud(data2$high, data2$low, data2$close, tenkan_period, kijun_period, senkou_period)

sapply(ichimoku_data, length)


# Convert ichimoku_data to a data.table
ichimoku_dt <- dt$as.data.table(ichimoku_data)

# Get the maximum length among all columns
max_length <- max(sapply(c(list(data2), ichimoku_data), nrow))

# Create a sequence of row numbers
row_nums <- seq_len(max_length)

# Bind the data tables, filling with NA where necessary
result <- data2[row_nums][, c(names(ichimoku_dt)) := ichimoku_dt[row_nums]]

# If you want to ensure specific column order, you can use setcolorder
setcolorder(result, c(names(data2), names(ichimoku_dt)))





data2[, `:=`(
    tenkan_sen = ichimoku_data$tenkan_sen,
    kijun_sen = ichimoku_data$kijun_sen,
    chikou_span = c(rep(NA, kijun_period), head(close, -kijun_period)),
    senkou_span_a = c(tail(ichimoku_data$senkou_span_a, .N - kijun_period), rep(NA, kijun_period)),
    senkou_span_b = c(tail(ichimoku_data$senkou_span_b, .N - kijun_period), rep(NA, kijun_period))
)]

# Prepare data for plotting
plot_data <- dt$melt(
    data2,
    id.vars = "datetime", 
    measure.vars = c("close", "tenkan_sen", "kijun_sen", "chikou_span", "senkou_span_a", "senkou_span_b"),
    variable.name = "indicator", value.name = "value"
)

# Create the plot
ggplot2$ggplot(as.data.frame(plot_data), ggplot2$aes(x = datetime, y = value, color = indicator)) +
    ggplot2$geom_ribbon(
        data = dt, ggplot2$aes(ymin = pmin(senkou_span_a, senkou_span_b),
        ymax = pmax(senkou_span_a, senkou_span_b),
        fill = senkou_span_a > senkou_span_b),
        alpha = 0.3, inherit.aes = FALSE
    ) +
    ggplot2$scale_fill_manual(values = c("red", "green"), guide = "none") +
    ggplot2$geom_line() +
    ggplot2$scale_color_manual(values = c("black", "blue", "red", "green", "brown", "purple")) +
    ggplot2$labs(
        title = "Ichimoku Cloud",
        x = "Date",
        y = "Price"
    )
