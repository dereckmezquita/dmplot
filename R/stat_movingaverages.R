
# df <- data.table::fread("data/kucoin_prices.csv")

StatMovingAverage <- ggplot2::ggproto(
    "StatMovingAverage",
    ggplot2::Stat,
    # allows user to chose which value as input; high, low, close etc.
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
    compute_group = \(data, scales, colour, FUN, n, wilder) {
        if (n > nrow(data)) rlang::abort(stringr::str_interp('Interval n: ${n} provided is larger than values in dataset nrow: ${nrow(data)}.'))

        data.table::setDT(data)

        # ... additional arguments for FUN
        FUN <- match.fun(FUN)
        # if wilder is not na use it in calculation
        data[, y := FUN(y, n = n, wilder = ifelse(is.na(wilder), FALSE, wilder))]

        data[, colour := colour]

        return(data)
    }
)

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
    FUN = NULL,
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


