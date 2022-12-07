
# df <- data.table::fread("data/kucoin_prices.csv")

StatMovingAverage <- ggplot2::ggproto(
    "StatMovingAverage",
    ggplot2::Stat,
    # allows user to chose which value as input; high, low, close etc.
    required_aes = "x",
    default_aes = ggplot2::aes(
        short = NULL,
        long = NULL,
        y = NULL
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
    compute_group = \(data, scales, colour, interval) {
        data.table::setDT(data)

        data[, y := get(interval)]
        data[, colour := colour]

        return(data)
    }
)

#' @title Moving averages `ggplot2` layer
#' @author Dereck de Mezquita
#' 
#' @description 
#' `stat_movingaverages` is a `ggplot2` layer that allows you to plot moving averages on a `ggplot2` plot either by providing the column names `ggplot2::aes` of the previously calculated metrics.
#' 
#' You are free to use whatever algorithm you desire; the result will be two line plots one for a short moving average and one for a long moving average.
#' 
#' To use this layer provide `ggplot2::aes` values for `x` (datetime x-axis) and `short` and `long` (y-axis).
#' 
#' See examples for more details.
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' @param data A `data.table` object (required - default `NULL`).
#' @param size A `numeric` vector of length one; the size of the line (optional - default `1.75`).
#' @param alpha A `numeric` vector of length one; the alpha of the line (optional - default `0.75`).
#' @param colour A named or unnamed `list` with three elements "short" and "long". These are the colours for the short and long moving averages (optional - default `list(short = "red", long = "blue")`).
#' 
#' @section Aesthetics:
#' \code{stat_movingaverages} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x} -- datetime (x-axis)
#'   \item \strong{short} -- the values for the short moving average (y-axis).
#'   \item \strong{long} -- the values for the long moving average (y-axis)
#' }
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
#' ema <- function(close, n = 2, wilder = TRUE) {
#'     return(as.list(as.data.frame(TTR::EMA(close, n = n, wilder = wilder))))
#' }
#' 
#' # calculate the short and long moving averages
#' dt[, ema_short := ema(close, n = 10, wilder = TRUE)]
#' dt[, ema_long := ema(close, n = 50, wilder = TRUE)]
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
#'     # provide the colnames to the calculated indicators as aes values
#'     ddplot::stat_movingaverages(ggplot2::aes(short = ema_short, long = ema_long), alpha = list(mavg = 0.5)) +
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

stat_movingaverages <- function(
    mapping = NULL,
    data = NULL,
    geom = "line",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    size = 1.75,
    alpha = 0.75,
    n = list(
        short = 20L,
        long = 200L
    ),
    colour = list(
        short = "yellow",
        long = "purple"
    ),
    ...
) {
    list(
        ggplot2::layer(
            stat = StatMovingAverage,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                size = size,
                colour = colour$short,
                interval = "short",
                ...
            )
        ),
        ggplot2::layer(
            stat = StatMovingAverage,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                size = size,
                colour = colour$long,
                interval = "long",
                ...
            )
        )
    )
}


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
#     # y = close can go in ggplot2 call or here
#     # allows user to choose which values to use for calculations
#     # accepts wilder as an additional argument
#     stat_movingaverages(ggplot2::aes(y = close), FUN = TTR::SMA)


