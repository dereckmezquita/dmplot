
# df <- data.table::fread("data/kucoin_prices.csv")

StatMovingAverage <- ggplot2::ggproto(
    "StatMovingAverage",
    ggplot2::Stat,
    # allows user to chose which value as input; high, low, close etc.
    required_aes = c("x", "FUN", "n"),
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
    compute_group = \(data, scales, colour, FUN, n, wilder, interval) {
        data.table::setDT(data)

        # if FUN provided
        if (!is.null(FUN)) {
            if (n > nrow(data)) rlang::abort(stringr::str_interp('Interval n: ${n} provided is larger than values in dataset nrow: ${nrow(data)}.'))

            # ... additional arguments for FUN
            FUN <- match.fun(FUN)
            # if wilder is not na use it in calculation
            if (!is.na(wilder)) {
                data[, y := FUN(y, n = n, wilder = wilder)]
            } else {
                data[, y := FUN(y, n = n)]
            }
        } else if (all(c("short", "long") %in% colnames(data))) {
            # if short and long provided in data as aes
            data[, y := get(interval)]
        }

        data[, colour := colour]

        return(data)
    }
)

#' @title Moving averages `ggplot2` layer
#' @author Dereck de Mezquita
#' 
#' @description 
#' `stat_movingaverages` is a `ggplot2` layer that allows you to plot moving averages on a `ggplot2` plot either by providing a function to calculate the moving averages or by providing the column names `ggplot2::aes` of the previously calculated moving averages.
#' 
#' You are free to use whatever algorithm you desire, as long as:
#' 
#' 1. If using a function it must return a `numeric` vector of the same length as the input vector and accept the arguments `x` (`numeric` vector of value) `n` (`numeric` interval of time used to calculate the moving average); an optional `wilder` argument applicable to `ema` type functions who use the wilder smoothing method.
#' 
#' 2. If using previously calculated moving averages, you must provide them as `ggplot2::aes` values; the `aes` must be `short` and `long`.
#' 
#' See examples for more details.
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' @param data A `data.table` object (required - default `NULL`).
#' @param FUN A `function` to calculate the moving average (can be simple/exponential or any algorithm). The only requirement is that the function a `numeric` vector as a result. The interval value is defined as a secondary argument to this stat see argument `n` (required - default `NULL`).
#' @param n A `list` with two elements "short" and "long". These are the intervals for the short and long moving averages (note if these are longer than data points provided you see an error) (required - default `list(short = 20L, long = 200L)`).
#' @param wilder A `logical` of length one; if set this argument is passed on to the `FUN` which should be able to accept it - for choosing between a classic EMA or a Wilder's EMA (optional - default `NA`).
#' @param size A `numeric` vector of length one; the size of the line (optional - default `1.75`).
#' @param alpha A `numeric` vector of length one; the alpha of the line (optional - default `0.75`).
#' @param colours A `list` with three elements "short" and "long". These are the colours for the short and long moving averages (optional - default `list(short = "red", long = "blue")`).
#' 
#' @section Aesthetics:
#' \code{stat_movingaverages} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x} -- datetime (x-axis)
#'   \item y -- required if using `FUN` to calculate moving averages (y-axis)
#'   \item short -- required if not using `FUN` to calculate moving averages (y-axis)
#'   \item long -- required if not using `FUN` to calculate moving averages (y-axis)
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
#' bb <- function(close, n = 2, sd = 2) {
#'     return(as.list(as.data.frame(TTR::BBands(close, n = n, sd = sd))))
#' }
#' 
#' ema <- function(close, n = 2, wilder = TRUE) {
#'     return(as.list(as.data.frame(TTR::EMA(close, n = n, wilder = wilder))))
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
#'     # ddplot::stat_bollingerbands(ggplot2::aes(y = close), FUN = bb, alpha = list(mavg = 0.5, ribbon = 0.25)) +
#'     ddplot::stat_movingaverages(ggplot2::aes(y = close), FUN = ema, n = list(short = 10, long = 50), alpha = list(mavg = 0.5)) +
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
#' ## ------------------------------------
#' # you can also provide the column names as aes instead of calculating them by the passing of a function
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
    FUN = NULL, # by default uses the same value input with no calculations
    n = list(
        short = 20L,
        long = 200L
    ),
    colours = list(
        short = "yellow",
        long = "purple"
    ),
    wilder = NA,
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
                FUN = FUN,
                n = n$short,
                size = size,
                colour = colours$short,
                wilder = wilder,
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
                FUN = FUN,
                n = n$long,
                size = size,
                colour = colours$long,
                wilder = wilder,
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


