#' @export
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


#' @export
stat_movingaverages <- function(
    mapping = NULL,
    data = NULL,
    geom = "line",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    linewidth = 1.75,
    alpha = 0.75,
    colour = list(
        short = "yellow",
        long = "purple"
    ),
    ...) {
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
                linewidth = linewidth,
                colour = colour[[1]], # $short,
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
                linewidth = linewidth,
                colour = colour[[2]], # $long,
                interval = "long",
                ...
            )
        )
    )
}
