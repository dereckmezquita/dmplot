box::use(./helpers[...])

#' @export
Pca <- R6::R6Class(
    "Pca",
    public = list(
        prcomp = NULL, # prcomp class
        refined_prcomp = NULL,
        top_rotations = NULL,
        plot_params = NULL,
        scree = NULL,
        scatter = NULL,
        initialize = function(
            data,
            comparison,
            plot_params = list(
                scree = list(num_pc = 50),
                scatter = list(
                    point_size = 3,
                    point_alpha = 1,
                    point_labels = list(
                        show = TRUE,
                        size = 4,
                        max_overlaps = 10,
                        alpha = 0.75,
                        font_face = "bold"
                    ),
                    top_contributors = list(
                        show = TRUE,
                        truncate = 30
                    )
                )
            ),
            ... # passed on to private$calculate_prcomp()
        ) {
            private$check_args(data, comparison)

            self$plot_params <- plot_params
            private$calculate_prcomp(data, comparison, ...)
        },
        print = function() {
            cat("PCA object:\n")
            cat("-------------------------------\n")
            cat("Proportion of Variance Explained by each PC:\n")
            print(self$refined_prcomp[, .(PC, pct_var_explained)], digits = 4)
        },
        print_pca_explanation = function() {
            cat(crayon::blue("Detailed Explanation of PCA:\n"))
            cat(crayon::blue("-------------------------------\n"))

            cat(crayon::green("\n1. Standardization:\n"))
            cat("PCA starts with a dataset of n-dimensions (in this case, genes are dimensions\n",
                "and samples are observations). The data is standardized, transforming each dimension\n",
                "to have a mean of 0 and a standard deviation of 1.\n")

            cat(crayon::green("\n2. Covariance Matrix Computation:\n"))
            cat("A covariance matrix is computed. This matrix indicates the covariance\n",
                "(shared variance) between each pair of dimensions. The covariance between different\n",
                "dimensions is used to understand the correlation structure of the original dimensions.\n")

            cat(crayon::green("\n3. Eigendecomposition:\n"))
            cat("The covariance matrix is then decomposed into its eigenvectors and eigenvalues.\n",
                "Each eigenvector represents a principal component, which is a linear combination of\n",
                "the original dimensions. The associated eigenvalue represents the amount of variance\n",
                "explained by the principal component. The eigenvectors are ordered by their corresponding\n",
                "eigenvalues, so the first principal component (PC1) explains the most variance, followed by PC2, etc.\n")

            cat(crayon::green("\n4. Selection of Principal Components:\n"))
            cat("Depending on the goal of the analysis, some or all of the principal components can\n",
                "be selected for further analysis. The 'elbow method' is commonly used, where you plot\n",
                "the explained variance by each principal component and look for an 'elbow' in the plot as a cut-off point.\n")

            cat(crayon::green("\n5. Interpretation:\n"))
            cat("The 'top rotations' in the context of PCA refer to the features (genes) that contribute\n",
                "most to each principal component. The 'rotation' matrix from prcomp() gives the loadings\n",
                "of each feature onto each PC. By identifying features with large absolute loadings, we can\n",
                "understand what features drive the separation in the data along the principal components.\n",
                "In other words, the top rotations tell us which genes are most important for explaining\n",
                "the variance in our data along each PC.\n")
        },
        plot_scree = function(
            num_pc = self$plot_params$scree$num_pc,
            title = "Scree plot",
            subtitle = "Percent variance explained by each principal component",
            caption = stringr::str_interp('Top ${num_pc} PCs included'),
            replot = TRUE
        ) {
            self$scree <- ggplot2::ggplot(
                utils::head(self$refined_prcomp, num_pc),
                ggplot2::aes(x = PC, y = pct_var_explained)
            ) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::labs(
                title = title,
                subtitle = subtitle,
                x = "Principal component",
                y = "Percent variation",
                caption = caption
            ) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

            return(self$scree)
        },
        plot_scatter = function(
            comparison = NULL,
            point_default_colour = "grey",
            point_size = self$plot_params$scatter$point_size,
            point_alpha = self$plot_params$scatter$point_alpha,
            point_labels = list(
                show = self$plot_params$scatter$point_labels$show,
                size = self$plot_params$scatter$point_labels$size,
                max_overlaps = self$plot_params$scatter$point_labels$max_overlaps,
                alpha = self$plot_params$scatter$point_labels$alpha,
                font_face = self$plot_params$scatter$point_labels$font_face
            ),
            top_contributors = list(
                show = self$plot_params$scatter$top_contributors$show,
                truncate = self$plot_params$scatter$top_contributors$truncate
            ),
            title = if (!is.null(comparison)) stringr::str_interp('${comparison$comparison_name}: principal components 1 and 2') else "Principal components 1 and 2",
            subtitle = stringr::str_interp('${nrow(self$prcomp$x)} samples, ${ncol(self$prcomp$rotation)} principal components, calculated from ${nrow(self$prcomp$rotation)} features'),
            caption = if (top_contributors$show) stringr::str_interp('Top contributors to variance:\nPC1: ${paste0(stringr::str_trunc(names(self$top_rotations$PC1), top_contributors$truncate), collapse = ", ")}\nPC2: ${paste0(stringr::str_trunc(names(self$top_rotations$PC2), top_contributors$truncate), collapse = ", ")}') else NULL,
            colour_palette = "viridis",
            ...
        ) {
            pc <- data.table::data.table(self$prcomp$x, keep.rownames = "sample")

            if (!is.null(comparison)) {
                if (!inherits(comparison, "Comparison")) rlang::abort("comparison must be a Comparison object")

                pc <- merge(
                    pc,
                    comparison$comparison_table[, c("sample", "group")],
                    by = "sample",
                    all = FALSE
                )

                # if (comparison$test == "t-test") {
                    pc[
                        , highlight := data.table::fifelse(group %in% comparison$group_order[1], "blue", "red")
                    ]
                # } else if (comparison$test == "anova") {
                #     colours <- grDevices::hcl.colors(length(unique(pc$group)), palette = colour_palette)
                #     group_colours <- sort(unique(pc$group))

                #     names(colours) <- group_colours

                #     pc[, highlight := colours[group]]
                # }
            } else {
                pc[, highlight := point_default_colour]
            }

            self$scatter <- ggplot2::ggplot(
                    pc,
                    ggplot2::aes(
                        x = PC1,
                        y = PC2,
                        colour = highlight
                    )
                ) +
                ggplot2::geom_point(size = point_size, alpha = point_alpha) +
                {if (point_labels$show) {
                    suppressWarnings(ggrepel::geom_text_repel(
                        ggplot2::aes(label = sample),
                        size = point_labels$size,
                        max.overlaps = point_labels$max_overlaps,
                        alpha = point_labels$alpha,
                        fontface = point_labels$font_face
                    ))
                }} +
                ggplot2::scale_colour_identity() +
                ggplot2::labs(
                    title = title,
                    subtitle = subtitle,
                    x = paste('PC1 - ', self$refined_prcomp$pct_var_explained[1], '%', sep = ''),
                    y = paste('PC2 - ', self$refined_prcomp$pct_var_explained[2], '%', sep = ''),
                    colour = if (!is.null(comparison)) "Group" else NA,
                    caption = caption
                ) +
                ggplot2::theme(legend.position = "bottom")

            return(self$scatter)
        }
    ),
    private = list(
        calculate_prcomp = function(data, comparison = NULL, ...) {
            data <- data$clone(deep = TRUE)

            if (!is.null(comparison)) {
                if (!inherits(comparison, "Comparison")) rlang::abort("Comparison must be of class Comparison.")

                comparison <- comparison$clone(deep = TRUE)
                data_matrix <- data$set_filter_comparison_samples(comparison)$data
            } else {
                data_matrix <- data$data
            }

            data_matrix <- to.data.frame(data.table::transpose(
                data_matrix,
                keep.names = "sample",
                make.names = "feature"
            ), id.col = "sample")

            self$prcomp <- stats::prcomp(
                data_matrix,
                ...
            )

            # ---
            # Explanation: rotation and variance contributed
            # get the name of the top 3 measurements (features) that contribute most to pc1
            # "rotation" are the principal components (the eigenvectors of the covariance matrix), in the original coordinate system. Typically a square matrix (unless you truncate it by introducing tolerance) with the same number of dimensions your original data had. E.g. if you had a 3D data set, your rotation matrix will be 3-by-3.
            # get names only of top contributors per PC
            self$top_rotations <- (\(pcs) {
                annotated_rotations <- lapply(pcs, \(pc) {
                    return(utils::head(sort(abs(self$prcomp$rotation[, pc]), decreasing = TRUE), 3))
                })

                names(annotated_rotations) <- pcs
                return(annotated_rotations)
            })(paste("PC", 1:ncol(self$prcomp$rotation), sep = ""))
            # ---

            self$refined_prcomp <- private$refine_prcomp()
        },
        refine_prcomp = function() {
            # PCs to variance explained as percentage - df PC to var%
            var_explained <- data.table::data.table(
                PC = paste0("PC", 1:length(self$prcomp$sdev)), # ID PCs
                pct_var_explained = (\(pca_var) {
                    # warn if pca_var is 0s
                    if (all(pca_var == 0)) rlang::warn("All variance is 0. Check your data.")

                    # calculate percent variance
                    return(round(pca_var / sum(pca_var) * 100, 2))
                })(self$prcomp$sdev ^ 2) # Note: sdev ^ 2 = variance
            )

            refined <- merge(
                var_explained,
                data.table::transpose(
                    data.table::data.table(self$prcomp$x, keep.rownames = "sample"),
                    keep.names = "PC",
                    make.names = "sample"
                ),
                by = "PC",
                all = FALSE,
                sort = FALSE
            )

            # must be a factor so order ggplot2
            refined[, PC := factor(PC, PC)]

            return(refined[])
        },
        check_args = function(data, comparison) {
            if (!inherits(data, "Data")) {
                rlang::abort("data must be of class Data.")
            }

            if (!inherits(comparison, "Comparison")) {
                rlang::abort("comparison must be of class Comparison.")
            }
        }
    )
)