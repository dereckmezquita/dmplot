
StatMacdMovingAverage <- ggplot2::ggproto(
    "StatMacdMovingAverage",
    ggplot2::Stat,
    required_aes = c("x"),
    default_aes = ggplot2::aes(
        y = NULL,
        macd = NULL,
        macd_signal = NULL,
        macd_diff = NULL
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
    compute_group = \(data, scales, colour, alpha, macd_line_name) {
        data.table::setDT(data)

        data[, y := get(macd_line_name)]

        data[, colour := colour]
        data[, alpha := alpha]

        return(data)
    }
)

StatMacdHistogram <- ggplot2::ggproto(
    "StatMacdHistogram",
    ggplot2::Stat,
    required_aes = c("x"),
    default_aes = ggplot2::aes(
        y = NULL,
        macd = NULL,
        macd_signal = NULL,
        macd_diff = NULL
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
    compute_group = \(data, scales, colours, fills, alpha) {
        data.table::setDT(data)

        if (!"macd_diff" %in% colnames(data)) {
            data[, y := macd - macd_signal]
        } else {
            data[, y := macd_diff]
        }

        # 1 is positive and 2 is negative
        data[, colour := ifelse(y > 0, colours[[1]], colours[[2]])]
        data[, fill := ifelse(y > 0, fills[[1]], fills[[2]])]
        data[, alpha := alpha]

        return(data)
    }
)

#' @title Moving average convergence divergence (macd) `ggplot2` layer
#' @author Dereck de Mezquita
#' 
#' @description 
#' `stat_macd` is a `ggplot2` layer that allows you to plot a `ggplot2` layer of moving average convergence divergence (macd) by providing the column names `ggplot2::aes` of the previously calculated metrics.
#' 
#' You are free to use whatever algorithm you desire; the result will be a line plot overlayed on a histogram.
#' 
#' To use this layer provide `ggplot2::aes` values for `x` (datetime x-axis) and `macd` (y-axis) and `macd_signal` (y-axis) and `macd_diff` (y-axis).
#' 
#' See examples for more details.
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' @param data A `data.table` object (required - default `NULL`).
#' @param size A `numeric` vector of length one; the size of the line (optional - default `1.75`).
#' @param alpha_lines A `numeric` vector of length one; the alpha of the lines (optional - default `0.75`).
#' @param alpha_histogram A `numeric` vector of length one; the alpha of the histogram (optional - default `0.5`).
#' @param colour_lines A named or unnamed `list` of two elements "macd" and "macd_signal" (optional - default `list(macd = "blue", macd_signal = "red")`).
#' @param colour_histogram A named or unnamed `list` of two elements "positive" and "negative" (optional - default `list(positive = NA_character_, negative = NA_character_)`).
#' @param fill_histogram A named or unnamed `list` of two elements "positive" and "negative" (optional - default `list(positive = "green", negative = "red")`).
#' 
#' @section Aesthetics:
#' \code{stat_movingaverages} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x} -- datetime (x-axis)
#'   \item \strong{macd} -- y-axis
#'   \item \strong{macd_signal} -- y-axis
#'   \item \strong{macd_diff} -- y-axis
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
#' @export
stat_macd <- function(
    mapping = NULL,
    data = NULL,
    geom = "line",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    size = 1,
    alpha_lines = 0.75,
    alpha_histogram = 0.5,
    colour_lines = list( # we get colours by using [[]] allows for passing unnamed list
        macd = "blue",
        macd_signal = "red"
    ),
    colour_histogram = list(
        positive = NA_character_,
        negative = NA_character_
    ),
    fill_histogram = list(
        positive = "green",
        negative = "red"
    ),
    ...
) {
    list(
        ggplot2::layer(
            stat = StatMacdHistogram,
            data = data,
            mapping = mapping,
            geom = "bar",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                colours = colour_histogram,
                fills = fill_histogram,
                alpha = alpha_histogram,
                ...
            )
        ),
        ggplot2::layer(
            stat = StatMacdMovingAverage,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                size = size,
                colour = colour_lines[[1]], # taking macd element
                alpha = alpha_lines,
                macd_line_name = "macd",
                ...
            )
        ),
        ggplot2::layer(
            stat = StatMacdMovingAverage,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                size = size,
                colour = colour_lines[[2]], # taking macd_signal element
                alpha = alpha_lines,
                macd_line_name = "macd_signal",
                ...
            )
        )
    )
}
