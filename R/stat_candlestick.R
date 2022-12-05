
# https://www.reddit.com/r/Rlanguage/comments/yytgdm/create_custom_ggplot2_candlesticks_geom_based_on/
# df <- data.table::fread("data/kucoin_prices.csv")

StatCandleBarrel <- ggplot2::ggproto(
    "StatCandleBarrel",
    ggplot2::Stat,
    required_aes = c("x", "open", "close"),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, scales, colours) {
        data.table::setDT(data)

        data[, gain_loss := data.table::fcase(
            close > data.table::shift(close, 1L, type = "lag"), "up",
            close < data.table::shift(close, 1L, type = "lag"), "down",
            default = "no_change"
        )]

        candle_width <- min(data$x - data.table::shift(data$x, 1L, type = "lag"), na.rm = TRUE)

        data <- data.table::data.table(
            xmin = data$x - candle_width / 2 * 0.8,
            xmax = data$x + candle_width / 2 * 0.8,
            ymin = pmin(data$open, data$close),
            ymax = pmax(data$open, data$close),
            colour = unlist(colours[data$gain_loss]),
            fill = unlist(colours[data$gain_loss])
        )

        return(data)
    }
)

StatWick <- ggplot2::ggproto(
    "StatWick",
    ggplot2::Stat,
    required_aes = c("x", "high", "low"),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, scales, colours) {
        data.table::setDT(data)

        data[, gain_loss := data.table::fcase(
            close > data.table::shift(close, 1L, type = "lag"), "up",
            close < data.table::shift(close, 1L, type = "lag"), "down",
            default = "no_change"
        )]

        data[, c("ymax", "ymin") := list(high, low)]

        data[, colour := unlist(colours[data$gain_loss])]

        return(data)
    }
)

#' @export
stat_candlestick <- function(
    mapping = NULL,
    data = NULL,
    geom = "linerange",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    colours = list(
        up = "#55BE8B",
        down = "#ED4D5D",
        no_change = "#535453"
    ),
    ...
) {
    list(
        ggplot2::layer(
            stat = StatWick,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, colours = colours, ...)
        ),
        ggplot2::layer(
            stat = StatCandleBarrel,
            data = data,
            mapping = mapping,
            geom = "rect",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, colours = colours, ...)
        )
    )
}

# tail(df, 50) |>
#     ggplot2::ggplot(ggplot2::aes(
#         datetime,
#         open = open,
#         close = close,
#         high = high,
#         low = low,
#         group = symbol
#     )) +
#     stat_candlestick()
