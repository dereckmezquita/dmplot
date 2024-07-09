# https://www.reddit.com/r/Rlanguage/comments/yytgdm/create_custom_ggplot2_candlesticks_geom_based_on/
StatCandleBarrel <- ggplot2::ggproto(
    "StatCandleBarrel",
    ggplot2::Stat,
    required_aes = c("x", "open", "close", "high", "low"), # fix for #13
    dropped_aes = c("x", "open", "close", "high", "low", "gain_loss"), # fix for #11
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
    required_aes = c("x", "high", "low", "open", "close"), # fix for #13
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

#' @title Candlesticks financial `ggplot2` layer
#' @author Dereck Mezquita
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' \itemize{
#'   \item{`x`: The x-axis value, usually representing time.}
#'   \item{`open`: The opening price.}
#'   \item{`close`: The closing price.}
#'   \item{`high`: The highest price in the time range.}
#'   \item{`low`: The lowest price in the time range.}
#'   \item{`group`: (optional) The grouping variable.}
#' }
#' @param data A `data.table` object (required - default `NULL`).
#' @param colours A `list` with three elements "up", "down", and "no_change". These are the colours of the candlesticks when a positive change in price action, a negative change and no change respectively.
#' @param ... Additional arguments passed to `ggplot2::layer`.
#'
#' @details
#'
#' This is a `ggplot2` extension; it is used with the `+` operator for adding a layer to a `ggplot2` object.
#'
#' @return A `ggplot2::layer` object.
#'
#' @examples
#'
#' # get some financial data
#' # kucoin is a private package - you can use any data source
#' ticker <- "BTC/USDT"
#'
#' dt <- kucoin::get_market_data(
#'     symbols = ticker,
#'     from = "2022-11-28 15:29:43 EST", # lubridate::now() - lubridate::days(7),
#'     to = "2022-12-05 15:29:31 EST",# lubridate::now(),
#'     frequency = "1 hour"
#' )
#'
#' dt
#'
#' dt |>
#'     ggplot2::ggplot(ggplot2::aes(
#'         x = datetime,
#'         open = open,
#'         close = close,
#'         high = high,
#'         low = low,
#'         group = symbol
#'     )) +
#'     ## ------------------------------------
#'     dmplot::stat_candlestick() +
#'     ## ------------------------------------
#'     ggplot2::scale_x_continuous(n.breaks = 25, labels = \(x) {
#'         lubridate::floor_date(lubridate::as_datetime(x), "hours")
#'     }) +
#'     ggplot2::scale_y_continuous(n.breaks = 25) +
#'     ggplot2::labs(
#'         title = ticker,
#'         x = "Date",
#'         y = "Price (USD)"
#'     ) +
#'     dmplot::theme_dereck_dark() +
#'     ggplot2::theme(
#'         axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.925, hjust = 0.975),
#'         panel.grid.minor = ggplot2::element_blank()
#'     )
#'
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
