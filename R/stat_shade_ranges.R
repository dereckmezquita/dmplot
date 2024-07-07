#' StatShadedDateRange
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
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


#' Shaded Date Ranges ggplot2 layer
#'
#' This function creates a ggplot2 layer that shades date ranges based on grouped data.
#' It's particularly useful for highlighting specific periods in time series data.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}.
#'   Must include `x` for dates and `group` for categorizing ranges.
#' @param data The data to be displayed in this layer. If NULL, the default,
#'   the data is inherited from the plot data as specified in the call to \code{\link[ggplot2]{ggplot}}.
#' @param geom The geometric object to use display the data. Default is "linerange".
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function. Default is "identity".
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#'   If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   NA, the default, includes if any aesthetics are mapped.
#' @param inherit.aes If FALSE, overrides the default aesthetics,
#'   rather than combining with them.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.
#'   These are often aesthetics, used to set an aesthetic to a fixed value.
#'
#' @return A ggplot2 layer object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(data.table)
#'
#' # Create sample data
#' dt <- data.table(
#'     datetime = seq(as.POSIXct("2023-01-01"), as.POSIXct("2023-12-31"), by = "day"),
#'     close = runif(365, 100, 200),
#'     call = sample(c("buy", "sell", "none"), 365, replace = TRUE, prob = c(0.3, 0.3, 0.4))
#' )
#'
#' # Create plot with shaded ranges
#' ggplot(dt, aes(x = datetime)) +
#'     geom_line(aes(y = close), linewidth = 1.5, colour = "white") +
#'     stat_shade_ranges(aes(group = call, fill = call), alpha = 0.25) +
#'     scale_fill_manual(
#'         name = "Call",
#'         values = c("buy" = "green", "sell" = "red", "none" = "gray")
#'     ) +
#'     theme_minimal() +
#'     labs(title = "Stock Price with Buy/Sell Signals", x = "Date", y = "Price")
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
    ...) {
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
