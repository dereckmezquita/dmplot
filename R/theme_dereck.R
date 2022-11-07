#' @title Add derecksnotes theme to ggplot chart
#' @author Dereck de Mezquita
#'
#' This function allows you to add the derecksnotes theme to your ggplot graphics.
#' @keywords theme_derecksnotes
#' @importFrom ggplot2 '%+replace%'
#' @export
#' @examples
#' iris |>
#'     ggplot2::ggplot(ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
#'     ggplot2::geom_point(ggplot2::aes(color = Species, shape = Species)) +
#'     ggplot2::labs(title = "Iris data", subtitle = "Classic data", x = "Sepal Length", y = "Sepal Width") +
#'     ggplot2::facet_grid(~ Species) +
#'     ddplot::theme_dereck()
#'
#' data.frame(year = time(nhtemp), tmps = as.matrix(nhtemp)) |>
#'     ggplot2::ggplot(ggplot2::aes(x = year, y = tmps)) +
#'     ggplot2::geom_line() +
#'     # geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#'     ggplot2::labs(title = "New Hampshire yearly temperatures", subtitle = "Collected between 1912 and 1971", x = "Year", y = "Average temperature") +
#'     ddplot::theme_dereck()
#'

theme_dereck <- function () { # base_size = 11, base_family = ""
    ggplot2::theme_bw() %+replace% ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 1, margin = ggplot2::margin(0, 0, 5, 0, unit = "pt")), # 0.5 centres when position is set to "plot"
        plot.subtitle = ggplot2::element_text(hjust = 1, vjust = 0.015, margin = ggplot2::margin(0, 0, 5, 0, unit = "pt")),

        panel.background = ggplot2::element_blank(),
        # panel.border = ggplot2::element_rect(color = "#afafaf", fill = NA),
        axis.line = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),

        panel.grid.minor = ggplot2::element_line(color = "#454545", size = 0.135, linetype = "dotted"),
        panel.grid.major = ggplot2::element_line(color = "#aaaaaa", size = 0.175, linetype = "longdash"),

        # axis.line = ggplot2::element_line(color = "#afafaf"),
        axis.ticks = ggplot2::element_line(color = "#aaaaaa"),

        strip.background = ggplot2::element_rect(fill = "white", colour = "#676767"),

        legend.position = "bottom",
        legend.title = ggplot2::element_text(vjust = 0.85) # adjust title position higher to key when bottom
    )
}
