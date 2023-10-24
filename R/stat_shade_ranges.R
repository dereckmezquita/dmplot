
StatShadedDateRange <- ggplot2::ggproto(
    "StatShadedDateRange",
    ggplot2::Stat,
    required_aes = c("x", "group"),
    setup_params = \(data, params) {
        params <- params
        return(params)
    },
    setup_data = \(data, params) {
        data.table::setDT(data)
        # data[, grouping := as.numeric(as.factor(grouping))]
        data[, data.table::setorder(.SD, x), by = group]
        return(data)
    },
    compute_group = \(data, fill) {
        data.table::setDT(data)
        data[, fill := fill]
        return(data)
    },
    compute_panel = \(data, scales, fill = "white") {
        # https://bookdown.dongzhuoer.com/hadley/ggplot2-book/new-stats.html
        # setting compute panel; don't split by group
        data.table::setDT(data)

        # https://stackoverflow.com/questions/73308307/how-to-break-up-a-vector-into-contiguous-groups-in-r
        groups <- unique(data$group) # NAs are removed in setup_data/na.rm argument
        names(groups) <- groups

        date_ranges <- lapply(groups, \(group) {
            # get the indices of the rows where the group is the group
            indices <- which(data$group == group)
            # split the indices into contiguous groups; produces a list of vectors
            indices <- split(indices, cumsum(c(TRUE, diff(indices) != 1)))

            # go over each vector and get the min max dates; use external dataset
            return(lapply(indices, \(row) {
                return(data.table::data.table(
                    start = min(data[row, ]$x),
                    end = max(data[row, ]$x),
                    call = group
                ))
            }))
        })

        date_ranges <- data.table::rbindlist(unlist(date_ranges, recursive = FALSE))

        data.table::setorder(date_ranges, start)

        # set multiple columns on date_ranges
        date_ranges[, `:=`(
            xmin = start - 1,
            xmax = end,
            ymin = -Inf,
            ymax = Inf,
            group = call
        )]

        # merge data that has the fill column with date_ranges by group
        return(merge(
            unique(data[, .(group, fill)]),
            date_ranges,
            by = "group"
        ))
    }
)


#' @title Shaded Date Ranges `ggplot2` layer
#' @author Dereck Mezquita
#'
#' @param mapping A `ggplot2::aes` object (required - default `NULL`).
#' @param data A `data.table` object (required - default `NULL`).
#' @param ... Additional arguments passed to `ggplot2::layer`.
#'
#' @details
#'
#' This is a `ggplot2` extension; it is used with the `+` operator for adding a layer to a `ggplot2` object.
#' 
#' You can set the colour by fill aesthetic with a column.
#'
#' @return A `ggplot2::layer` object.
#' 
#' @examples
#' \dontrun{
#' dt |>
#'    ggplot2::ggplot(ggplot2::aes(x = datetime)) +
#'    ggplot2::geom_line(ggplot2::aes(y = close), linewidth = 1.5, colour = "white") +
#'    plots$stat_shade_ranges(ggplot2::aes(group = call, fill = call), alpha = 0.25) +
#'    ggplot2::scale_color_manual(
#'        name = "call",
#'        values = c("buy" = "green", "sell" = "red", "none" = "gray"),
#'        aesthetics = "fill"
#'   )
#' }
#'
#' @export
stat_shade_ranges <- function(
    mapping = NULL,
    data = NULL,
    geom = "linerange",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
    list(
        ggplot2::layer(
            stat = StatShadedDateRange,
            data = data,
            mapping = mapping,
            geom = "rect",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )
        )
    )
}