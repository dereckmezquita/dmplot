
# df <- data.table::fread("data/kucoin_prices.csv")

StatBollingerRibbon <- ggplot2::ggproto(
    "StatBollingerRibbon",
    ggplot2::Stat,
    required_aes = c("x", "y", "FUN", "n"),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, scales, colour, fill, FUN, n, sd) {
        data.table::setDT(data)

        FUN <- match.fun(FUN)

        # using cpp bb function
        # FUN must return list: "bb_lower", "bb_mavg", "bb_upper", "bb_pct"
        data[, c("ymin", "bb_mavg", "ymax", "bb_pct") := FUN(y, n = n, sd = sd)]

        data[, colour := colour]
        data[, fill := fill]

        return(data)
    }
)

StatBollingerMovingAverage <- ggplot2::ggproto(
    "StatBollingerMovingAverage",
    ggplot2::Stat,
    required_aes = c("x", "y", "FUN", "n"),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, scales, colour, FUN, n, sd) {
        data.table::setDT(data)

        FUN <- match.fun(FUN)

        # using cpp bb function
        # FUN must return list: "bb_lower", "bb_mavg", "bb_upper", "bb_pct"
        data[, c("bb_lower", "y", "bb_mavg", "bb_pct") := FUN(y, n = n, sd = sd)]

        data[, colour := colour]

        return(data)
    }
)


#' @title Bollinger bands `ggplot2` layer
#' @author Dereck de Mezquita
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' @param data A `data.table` object (required - default `NULL`).
#' @param FUN A `function` to calculate the Bollinger bands; must return a list (required - default `NULL`).
#' @param n A `vector` of length one; the number of periods to calculate the moving average (required - default `NULL`).
#' @param sd A `vector` of length one; the number of standard deviations to calculate the upper and lower bands (required - default `NULL`).
#' @param size A `list` with two elements "border" and "mavg". These are the line widths for the border and moving average lines (optional - default `list(border = 1, mavg = 1)`).
#' @param alpha A `list` with two elements "ribbon" and "mavg". These are the alpha values for the ribbon and moving average lines (optional - default `list(ribbon = 0.1, mavg = 0.5)`).
#' @param linetype A `list` with two elements "border" and "mavg". These are the line types for the border and moving average lines (optional - default `list(border = "dotted", mavg = 4)`).
#' @param colours A `list` with two elements "ribbon", "border", and "mavg". These are the colours for the ribbon, border, and moving average lines (optional - default `list(ribbon = "yellow", border = "magenta", mavg = "magenta")`).
#' @param ... Additional arguments passed to `ggplot2::layer`.
#' 
#' @details
#' 
#' This is a `ggplot2` extension; it is used with the `+` operator for adding a layer to a `ggplot2` object.
#'
#' @return A `ggplot2` layer.
#'
#' @examples
#' 
#' # get some financial data
#' # kucoin is private package - you can use any data source
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
#' # we need a function that calculates the indicator for us
#' # typically I like to write my own functions in C++; in this case we will use TTR's
#' # the stat expects a named list to be returned - we redefine ttr
#' bb <- function(close, n = 2, sd = 2) {
#'     return(as.list(as.data.frame(TTR::BBands(close, n = n, sd = sd))))
#' }
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
#'     ddplot::stat_candlestick() +
#'     ## ------------------------------------
#'     ddplot::stat_bollingerbands(ggplot2::aes(y = close), FUN = bb, alpha = list(mavg = 0.5, ribbon = 0.25)) +
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
#'     ddplot::theme_dereck_dark() +
#'     ggplot2::theme(
#'         axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.925, hjust = 0.975),
#'         panel.grid.minor = ggplot2::element_blank()
#'     )
#' 
#' @export
stat_bollingerbands <- function(
    mapping = NULL,
    data = NULL,
    geom = "ribbon",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    FUN = NULL,
    n = 20L,
    sd = 2L,
    size = list(
        border = 1,
        mavg = 1
    ),
    alpha = list(
        ribbon = 0.1,
        mavg = 0.5
    ),
    linetype = list(
        border = "dotted",
        mavg = 4
    ),
    colours = list(
        ribbon = "yellow",
        border = "magenta",
        mavg = "magenta"
    ),
    ...
) {
    list(
        ggplot2::layer(
            stat = StatBollingerRibbon,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                FUN = FUN,
                n = n,
                sd = sd,
                size = size$border,
                alpha = alpha$ribbon,
                linetype = linetype$border,
                fill = colours$ribbon,
                colour = colours$border,
                ...
            )
        ),
        ggplot2::layer(
            stat = StatBollingerMovingAverage,
            data = data,
            mapping = mapping,
            geom = "line",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                FUN = FUN,
                n = n,
                sd = sd,
                size = size$mavg,
                alpha = alpha$mavg,
                linetype = linetype$mavg,
                colour = colours$mavg,
                ...
            )
        )
    )
}


# if you wish to supply TTR::BBands you need to cast to list
# mybb <- function(close, n = 2, sd = 2) {as.list(as.data.frame(TTR::BBands(close, n = n, sd = sd)))}

# tail(df, 500) |>
#     ggplot2::ggplot(ggplot2::aes(
#         datetime,
#         open = open,
#         close = close,
#         high = high,
#         low = low,
#         group = symbol
#     )) +
#     stat_candlestick() +
#     stat_bollingerbands(ggplot2::aes(y = close), n = 20L, sd = 2L, FUN = mybb)
