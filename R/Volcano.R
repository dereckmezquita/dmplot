#' @importFrom R6 R6Class
#' @importFrom ggrepel geom_text_repel
#' @import ggplot2
#' @import data.table
#' @importFrom stringr str_interp
#' @importFrom utils head
#' @importFrom scales label_number
#' @export
Volcano <- R6Class(
    "Volcano",
    private = list(
        validate = \() {
            if(!all(c("feature", "log2FC") %in% colnames(self$data))) {
                stop(str_interp('The data must contain columns "feature", and "log2FC"; received ${colnames(self$data)}'))
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
        initialize = \(
            data = data.table(),
            statistic = "fdr",
            statistic_cutoff = 0.25,
            log2_cutoff = log2(1.5),
            head_labels = 10
        ) {
            self$data <- as.data.table(data)
            self$statistic <- statistic
            self$statistic_cutoff <- statistic_cutoff
            self$log2_cutoff <- log2_cutoff
            self$head_labels <- head_labels
            private$validate()
            self$process_data()
        },
        process = \() {
            copy <- data.table::copy(self$data)

            copy <- copy[
                order(abs(log2FC), -get(self$statistic), decreasing = c(TRUE, FALSE)),
            ]

            copy[, sig_label := c(utils::head(feature, self$head_labels), rep(NA_character_, nrow(copy) - self$head_labels))]

            copy[, highlight := fcase(
                log2FC < -self$log2_cutoff & get(self$statistic) < self$statistic_cutoff, "blue",
                log2FC > self$log2_cutoff & get(self$statistic) < self$statistic_cutoff, "red",
                default = "black"
            )]

            self$data <- copy
        },
        plot = function(
            plot_title,
            plot_subtitle,
            plot_caption,
            plot_y_lab,
            plot_x_lab,
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
                plot_caption <- str_interp('${n_significant}/${nrow(self$data)} signficant; ${n_down} down, ${n_up} up\n${self$statistic}: ${self$statistic_cutoff}, log2FC: ${round(self$log2_cutoff, 4)}, linear FC: ${round(2 ^ self$log2_cutoff, 4)}')
            }

            if (!missing(plot_y_lab) && !is.na(plot_y_lab) && plot_y_lab == "") {
                plot_y_lab <- str_interp('-log10(${self$statistic})')
            }

            if (!missing(plot_x_lab) && !is.na(plot_x_lab) && plot_x_lab == "") {
                plot_x_lab <- 'log2(fold change)'
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
                    # scales::label_number_si(accuracy = 0.005)(x)
                    scales::label_number(accuracy = 0.005)(x) # , scale_cut = scales::cut_si("unit")
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
