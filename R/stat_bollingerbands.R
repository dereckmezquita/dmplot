
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
