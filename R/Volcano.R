#' Volcano Plot R6 Class
#'
#' @description
#' An R6 class for creating and managing volcano plots for differential expression analysis.
#' This class provides methods for data preparation, plotting, and customisation of volcano plots.
#'
#' @details
#' The Volcano class uses differential expression data to create volcano plots,
#' highlighting significantly up- and down-regulated features based on log2 fold change
#' and a chosen statistical measure (e.g., FDR or p-value).
#'
#' @field data A data.table containing the differential expression data.
#' @field statistic Character. The statistic used for significance (default: "fdr").
#' @field statistic_cutoff Numeric. The cutoff value for the chosen statistic (default: 0.25).
#' @field log2_cutoff Numeric. The log2 fold change cutoff for significance (default: log2(1.5)).
#' @field head_labels Integer. The number of top features to label in the plot (default: 10).
#'
#' @export
Volcano <- R6::R6Class(
    "Volcano",
    private = list(
        validate = \() {
            if(!all(c("feature", "log2FC") %in% colnames(self$data))) {
                stop(stringr::str_interp('The data must contain columns "feature", and "log2FC"; received ${colnames(self$data)}'))
            }

            if(sum(c("fdr", "p_value") %in% colnames(self$data)) != 2) {
                stop(paste('The data must contain columns "fdr", and or "p_value"; received ', paste(colnames(self$data), collapse = ", ")))
            }
        }
    ),
    public = list(
        data = NULL,
        statistic = "fdr",
        statistic_cutoff = 0.25,
        log2_cutoff = log2(1.5),
        head_labels = 10,

        #' @description
        #' Create a new Volcano object.
        #' @param data A data.table containing differential expression data.
        #' @param statistic Character. The statistic used for significance (default: "fdr").
        #' @param statistic_cutoff Numeric. The cutoff value for the chosen statistic (default: 0.25).
        #' @param log2_cutoff Numeric. The log2 fold change cutoff for significance (default: log2(1.5)).
        #' @param head_labels Integer. The number of top features to label in the plot (default: 10).
        initialize = \(
            data = data.table(),
            statistic = "fdr",
            statistic_cutoff = 0.25,
            log2_cutoff = log2(1.5),
            head_labels = 10
        ) {
            self$data <- data.table::as.data.table(data)
            self$statistic <- statistic
            self$statistic_cutoff <- statistic_cutoff
            self$log2_cutoff <- log2_cutoff
            self$head_labels <- head_labels
            private$validate()
            self$process()
        },

        #' @description
        #' Process the input data for plotting.
        process = \() {
            copy <- data.table::copy(self$data)

            copy <- copy[
                base::order(abs(log2FC), -get(self$statistic), decreasing = c(TRUE, FALSE)),
            ]

            copy[, sig_label := c(utils::head(feature, self$head_labels), rep(NA_character_, nrow(copy) - self$head_labels))]

            copy[, highlight := data.table::fcase(
                log2FC < -self$log2_cutoff & get(self$statistic) < self$statistic_cutoff, "blue",
                log2FC > self$log2_cutoff & get(self$statistic) < self$statistic_cutoff, "red",
                default = "black"
            )]

            self$data <- copy
        },

        #' @description
        #' Create the volcano plot.
        #' @param plot_title Character. Title of the plot.
        #' @param plot_subtitle Character. Subtitle of the plot.
        #' @param plot_caption Character. Caption for the plot.
        #' @param plot_y_lab Character. Label for y-axis.
        #' @param plot_x_lab Character. Label for x-axis.
        #' @param legend_position Character. Position of the legend (default: "bottom").
        #' @param plot_theme Character. Theme for the plot (default: "dark").
        #' @param label Logical. Whether to add labels to points (default: TRUE).
        #' @return A ggplot object representing the volcano plot.
        plot_volcano = function(
            plot_title = "Volcano Plot",
            plot_subtitle = "",
            plot_caption = "",
            plot_y_lab = stringr::str_interp('-log10(${self$statistic})'),
            plot_x_lab = 'log2(fold change)',
            legend_position = "bottom",
            plot_theme = "dark",
            label = TRUE
        ) {
            n_significant <- sum(self$data$highlight != "black")
            n_down <- sum(self$data$highlight == "blue")
            n_up <- sum(self$data$highlight == "red")

            if (plot_title == "") {
                plot_title <- NULL
            }

            if (plot_subtitle == "") {
                plot_subtitle <- NULL
            }

            if (!missing(plot_caption) && !is.na(plot_caption) && plot_caption == "") {
                plot_caption <- stringr::str_interp('${n_significant}/${nrow(self$data)} signficant; ${n_down} down, ${n_up} up\n${self$statistic}: ${self$statistic_cutoff}, log2FC: ${round(self$log2_cutoff, 4)}, linear FC: ${round(2 ^ self$log2_cutoff, 4)}')
            }

            if (!missing(plot_y_lab) && !is.na(plot_y_lab) && plot_y_lab == "") {
                plot_y_lab <- stringr::str_interp('-log10(${self$statistic})')
            }

            self$data |>
                ggplot2::ggplot(ggplot2::aes(x = log2FC, y = -log10(get(self$statistic)), colour = highlight)) +
                ggplot2::geom_point(alpha = 0.5, size = 2) +
                ggplot2::scale_color_identity(
                    guide = "legend",
                    labels = c("Up-regulated", "Not significant", "Down-regulated"),
                    breaks = c("red", "black", "blue")
                ) +
                ggplot2::scale_y_continuous(n.breaks = 10, labels = function(x) {
                    scales::label_number(accuracy = 0.005)(x)
                }) +
                {if (any(-log10(self$data[["fdr"]]) > -log10(self$statistic_cutoff))) {
                    ggplot2::geom_hline(yintercept = -log10(self$statistic_cutoff), linetype = "dashed", colour = "goldenrod", linewidth = 0.75, alpha = 0.5)
                }} +
                {if (any(self$data$log2FC > self$log2_cutoff)) {
                    ggplot2::geom_vline(xintercept = self$log2_cutoff, linetype = "dashed", colour = "red", linewidth = 0.75, alpha = 0.5)
                }} +
                {if (any(self$data$log2FC < -self$log2_cutoff)) {
                    ggplot2::geom_vline(xintercept = -self$log2_cutoff, linetype = "dashed", colour = "blue", linewidth = 0.75, alpha = 0.5)
                }} +
                {if (label > 0) {
                    ggrepel::geom_text_repel(ggplot2::aes(label = sig_label), show.legend = FALSE, na.rm = TRUE)
                }} +
                ggplot2::labs(
                    title = plot_title,
                    subtitle = plot_subtitle,
                    caption = plot_caption,
                    x = plot_x_lab,
                    y = plot_y_lab
                ) +
                {if (plot_theme == "light") {
                    theme_dereck_light()
                } else if (plot_theme == "dark") {
                    theme_dereck_dark()
                } else if (plot_theme == "minimal") {
                    ggplot2::theme_minimal()
                }} +
                ggplot2::theme(legend.position = legend_position)
        }
    )
)
