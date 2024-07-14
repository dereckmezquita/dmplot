#' @export
Pca <- R6::R6Class(
    "Pca",
    private = list(
        filter_comparison_samples = function(data, comparison) {
            selected_samples <- colnames(data) %in% comparison$comparison_table$sample
            selected_samples[1] <- TRUE # keep GeneID/feature col

            return(data[, ..selected_samples][])
        },
        refine_prcomp = function() {
            # PCs to variance explained as percentage - df PC to var%
            var_explained <- data.table::data.table(
                PC = paste0("PC", 1:length(self$prcomp_results$sdev)), # ID PCs
                pct_var_explained = (\(pca_var) {
                    # warn if pca_var is 0s
                    if (all(pca_var == 0)) {
                        rlang::warn("All variance is 0. Check your data.")
                    }

                    # calculate percent variance
                    return(round(pca_var / sum(pca_var) * 100, 2))
                })(self$prcomp_results$sdev ^ 2) # Note: sdev ^ 2 = variance
            )

            refined <- merge(
                var_explained,
                data.table::transpose(
                    self$prcomp_results$x,
                    keep.names = "PC",
                    make.names = "sample"
                ),
                by = "PC",
                all = FALSE,
                sort = FALSE
            )

            # must be a factor so order ggplot2
            refined[, PC := factor(PC, PC)]

            self$prcomp_refined <- refined
        },
        prepare_data = function(data) {
            data <- data.table::copy(data)
            # drop non-numerical columns
            not_num <- sapply(data, \(col) {
                return(!is.numeric(col))
            })
            if (any(not_num[-1])) {
                rlang::warn("Dropping non-numeric columns from data.")
                selected_col <- c("feature", colnames(data)[!not_num])
                data <- data[, ..selected_col]
            }

            return(data[])
        },
        check_data = function(data) {
            if (!inherits(data, c("data.table", "data.frame"))) {
                rlang::abort("data must be of class Data.")
            }

            if (!is.character(data[[1]])) {
                rlang::abort("The first column must be type character, these are feature names.")
            }

            if (colnames(data)[1] != "feature") {
                rlang::abort("The first column must be named 'feature'.")
            }

            check_na <- valueCoordinates(data, value = NA)
            if (!is.empty(check_na)) {
                check_na_print <- printCapture(check_na)
                rlang::warn(stringr::str_interp('NA values found in data please review your data: ${check_na_print}'))
            }
        },
        check_comparison = function(comparison) {
            # comparison must be of class comparison or it can be NULL
            if (!inherits(comparison, "Comparison")) {
                rlang::abort("comparison must be a Comparison object.")
            }
        },
        validate = function() {
            private$check_data(self$data)
            if (!is.null(self$comparison)) {
                private$check_comparison(self$comparison)
            }
        }
    ),
    public = list(
        data = NULL,
        comparison = NULL,
        prcomp_results = NULL, # prcomp class
        prcomp_refined = NULL,
        top_rotations = NULL,
        scatter = NULL,
        scree = NULL,
        initialize = function(
            data,
            comparison = NULL
        ) {
            data <- data.table::copy(data)

            private$check_data(data)

            if (!is.null(comparison)) {
                private$check_comparison(comparison)

                data <- private$filter_comparison_samples(
                    data,
                    comparison
                )
            }

            self$data <- private$prepare_data(data)
            self$comparison <- comparison
        },
        prcomp = function(...) {
            data <- data.table::copy(self$data)

            data <- to.data.frame(data.table::transpose(
                data,
                keep.names = "sample",
                make.names = "feature"
            ), id.col = "sample")

            prcomp_results <- stats::prcomp(
                data,
                ...
            )

            # ---
            # Explanation: rotation and variance contributed
            # get the name of the top 3 measurements (features) that contribute most to pc1
            # "rotation" are the principal components (the eigenvectors of the covariance matrix), in the original coordinate system. Typically a square matrix (unless you truncate it by introducing tolerance) with the same number of dimensions your original data had. E.g. if you had a 3D data set, your rotation matrix will be 3-by-3.
            # get names only of top contributors per PC
            self$top_rotations <- (\(pcs) {
                annotated_rotations <- lapply(pcs, \(pc) {
                    # take top 3 contributors; abs, sort, head
                    return(head(sort(abs(prcomp_results$rotation[, pc]), decreasing = TRUE), 3))
                })

                names(annotated_rotations) <- pcs
                return(annotated_rotations)
            })(paste("PC", 1:ncol(prcomp_results$rotation), sep = ""))
            # ---
            prcomp_results$rotation <- data.table::as.data.table(
                prcomp_results$rotation,
                keep.rownames = "feature"
            )

            prcomp_results$x <- data.table::as.data.table(
                prcomp_results$x,
                keep.rownames = "sample"
            )

            self$prcomp_results <- prcomp_results
            private$refine_prcomp()
        },
        print = function() {
            cat("PCA object:\n")
            cat("-------------------------------\n")
            if (is.null(self$prcomp_results)) {
                cat("No PCA results available. Run Pca$prcomp() to compute PCA.\n")
            } else {
                cat("Proportion of Variance Explained by each PC:\n")
                print(self$prcomp_refined[, .(PC, pct_var_explained)], digits = 4)
            }
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
        plot_scree = function(num_pc = 50) {
            self$scree <- ggplot2::ggplot(
                    utils::head(self$prcomp_refined, num_pc),
                    ggplot2::aes(x = PC, y = pct_var_explained)
                ) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::labs(
                    title = "Scree plot",
                    subtitle = "Percent variance explained by each principal component",
                    x = "Principal component",
                    y = "Percent variation",
                    caption = stringr::str_interp('Top ${num_pc} PCs included')
                ) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

            return(self$scree)
        },
        plot_scatter = function(
            point_default_colour = "grey",
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
            ),
            title = if (!is.null(self$comparison)) stringr::str_interp('${self$comparison$comparison_name}: principal components 1 and 2') else "Principal components 1 and 2",
            subtitle = stringr::str_interp('${nrow(self$prcomp_results$x)} samples, ${ncol(self$prcomp_results$rotation)} principal components, calculated from ${nrow(self$prcomp_results$rotation)} features'),
            caption = if (top_contributors$show) stringr::str_interp('Top contributors to variance:\nPC1: ${paste0(stringr::str_trunc(names(self$top_rotations$PC1), top_contributors$truncate), collapse = ", ")}\nPC2: ${paste0(stringr::str_trunc(names(self$top_rotations$PC2), top_contributors$truncate), collapse = ", ")}') else NULL,
            ...
        ) {
            pc <- data.table::data.table(
                self$prcomp_results$x, keep.rownames = "sample"
            )

            comparison <- self$comparison

            if (!is.null(comparison)) {
                private$check_comparison(comparison)

                pc <- merge(
                    pc,
                    comparison$comparison_table[, c("sample", "group")],
                    by = "sample",
                    all = FALSE
                )

                pc[, highlight := data.table::fifelse(
                    group %in% comparison$group_order[1], "blue", "red"
                )]
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
                    x = paste('PC1 - ', self$prcomp_refined$pct_var_explained[1], '%', sep = ''),
                    y = paste('PC2 - ', self$prcomp_refined$pct_var_explained[2], '%', sep = ''),
                    colour = if (!is.null(comparison)) "Group" else NA,
                    caption = caption
                ) +
                ggplot2::theme(legend.position = "bottom")

            return(self$scatter)
        }
    )
)
