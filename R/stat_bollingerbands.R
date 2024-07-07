
# df <- data.table::fread("data/kucoin_prices.csv")

StatBollingerRibbon <- ggplot2::ggproto(
    "StatBollingerRibbon",
    ggplot2::Stat,
    required_aes = "x",
    default_aes = ggplot2::aes(
        y = NULL,
        ymin = NULL,
        ymax = NULL,
        mavg = NULL
    ),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, scales, colour, fill) {
        data.table::setDT(data)

        data[, colour := colour]
        data[, fill := fill]

        return(data)
    }
)

StatBollingerMovingAverage <- ggplot2::ggproto(
    "StatBollingerMovingAverage",
    ggplot2::Stat,
    required_aes = "x",
    default_aes = ggplot2::aes(
        y = NULL,
        ymin = NULL,
        ymax = NULL,
        mavg = NULL
    ),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, scales, colour) {
        data.table::setDT(data)

        data[, y := mavg]
        data[, colour := colour]

        return(data)
    }
)

#' @title Bollinger bands `ggplot2` layer
#' @author Dereck Mezquita
#' 
#' @description 
#' `stat_bollingerbands` is a `ggplot2` layer that allows you to plot Bollinger bands on a `ggplot2` plot either by providing a function to calculate the bands or by providing the column names of the required metrics as a `ggplot2::aes` argument previously calculated.
#' 
#' You are free to use whatever algorithm/function you wish as long as:
#' 
#' 1. If using a function it must return a named `list` - and ordered - with the following elements: `"bb_lower"`, `"bb_mavg"`, `"bb_upper"`, `"bb_pct"`.
#' 
#' 2. If using previously calculated metrics you must provide them as `ggplot2::aes` values; the `aes` must be `ymin`, `mavg`, `ymax`.
#' 
#' See examples for more details.
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' @param data A `data.table` object (required - default `NULL`).
#' @param linewidth A `list` with two elements "border" and "mavg". These are the line widths for the border and moving average lines (optional - default `list(border = 1, mavg = 1)`).
#' @param alpha A `list` with two elements "ribbon" and "mavg". These are the alpha values for the ribbon and moving average lines (optional - default `list(ribbon = 0.1, mavg = 0.5)`).
#' @param linetype A `list` with two elements "border" and "mavg". These are the line types for the border and moving average lines (optional - default `list(border = "dotted", mavg = 4)`).
#' @param colours A `list` with two elements "ribbon", "border", and "mavg". These are the colours for the ribbon, border, and moving average lines (optional - default `list(ribbon = "yellow", border = "magenta", mavg = "magenta")`).
#' @param ... Additional arguments passed to `ggplot2::layer`.
#' 
#' @section Aesthetics:
#' \code{stat_movingaverages} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x} -- datetime (x-axis)
#'   \item ymin -- required lower bounds of band (y-axis)
#'   \item ymax -- required upper bounds of band (y-axis)
#'   \item mavg -- required center line of band (y-axis)
#' }
#' 
#' @details
#' 
#' This is a `ggplot2` extension; it is used with the `+` operator for adding a layer to a `ggplot2` object.
#'
#' @return A `ggplot2` layer.
#'
#' @examples
#' \donttest{
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
#' bb <- function(close, n = 10, sd = 2) {
#'     return(as.list(as.data.frame(TTR::BBands(close, n = n, sd = sd))))
#' }
#' 
#' # calculate the short and long moving averages
#' dt[, c("bb_lower", "bb_mavg", "bb_upper", "bb_pct") := bb(close, n = 10, sd = 2)]
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
#'     # provide the colnames to the calculated indicators as aes values
#'     dmplot::stat_bollingerbands(ggplot2::aes(ymin = bb_lower, mavg = bb_mavg, ymax = bb_upper), colour = list("pink", "cyan", "cyan")) +
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
#' }
#' @export
stat_bollingerbands <- function(
    mapping = NULL,
    data = NULL,
    geom = "ribbon",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    # linewidth = list(
    #     border = 1,
    #     mavg = 1
    # ),
    alpha = list(
        ribbon = 0.1,
        mavg = 0.5
    ),
    linetype = list(
        border = "dotted",
        mavg = 4
    ),
    colour = list(
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
                # linewidth = linewidth[[1]],
                alpha = alpha[[1]],
                linetype = linetype[[1]],
                fill = colour[[1]],
                colour = colour[[2]],
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
                # linewidth = linewidth[[2]],
                alpha = alpha[[2]],
                linetype = linetype[[2]],
                colour = colour[[3]],
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
