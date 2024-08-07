#' @title Principal Component Analysis (PCA) Class
#' @description The `Pca` class implements Principal Component Analysis, a dimensionality reduction
#' technique widely used in data analysis and visualisation. This class provides methods
#' for performing PCA on a dataset, visualising the results, and interpreting the output.
#'
#' @details
#' PCA is a powerful technique for analysing high-dimensional data, such as gene expression
#' data in bioinformatics. It works by transforming the data into a new coordinate system
#' where the axes (principal components) are ordered by the amount of variance they explain.
#'
#' The PCA process involves several steps:
#'
#' 1. Standardisation:
#'    PCA begins with a dataset of n-dimensions (in the below demonstration, genes are
#'    dimensions and samples are observations). The data is standardised, transforming
#'    each dimension to have a mean of 0 and a standard deviation of 1.
#'
#' 2. Covariance Matrix Computation:
#'    A covariance matrix is computed. This matrix indicates the covariance
#'    (shared variance) between each pair of dimensions. The covariance between different
#'    dimensions is used to understand the correlation structure of the original dimensions.
#'
#' 3. Eigendecomposition:
#'    The covariance matrix is then decomposed into its eigenvectors and eigenvalues.
#'    Each eigenvector represents a principal component, which is a linear combination of
#'    the original dimensions. The associated eigenvalue represents the amount of variance
#'    explained by the principal component. The eigenvectors are ordered by their corresponding
#'    eigenvalues, so the first principal component (PC1) explains the most variance, followed by PC2, etc.
#'
#' 4. Selection of Principal Components:
#'    Depending on the goal of the analysis, some or all of the principal components can
#'    be selected for further analysis. The 'elbow method' is commonly used, where you plot
#'    the explained variance by each principal component and look for an 'elbow' in the plot as a cut-off point.
#'
#' 5. Interpretation:
#'    The 'top rotations' in the context of PCA refer to the features (genes) that contribute
#'    most to each principal component. The 'rotation' matrix from prcomp() gives the loadings
#'    of each feature onto each PC. By identifying features with large absolute loadings, we can
#'    understand what features drive the separation in the data along the principal components.
#'    In other words, the top rotations tell us which genes are most important for explaining
#'    the variance in our data along each PC.
#'
#' This class provides methods for each step of the PCA process, from data preparation
#' to visualisation of results. It's designed to work with any kind of high-dimensional
#' numerical data, as long as the data is in a tabular format with features as rows and
#' samples as columns. The first column must be named "feature" and contain the feature names.
#'
#' @field data The input data for PCA, typically a data.table with features as rows and samples as columns
#' @field comparison An optional Comparison object for group comparisons
#' @field prcomp_results Results from the stats::prcomp function, containing the raw PCA output
#' @field prcomp_refined Refined PCA results, including percentage of variance explained by each PC
#' @field top_rotations Top contributors (features) to each principal component
#' @field scatter The scatter plot of the first two principal components
#' @field scree The scree plot showing variance explained by each PC
#'
#' @examples
#' # Load required packages
#' box::use(dmplot[Pca, Comparison])
#' 
#' # Load example data
#' data(feature_counts, package = "dmplot")
#' 
#' # Prepare the data
#' data <- feature_counts[GeneBiotype == "protein_coding", ]
#' colnames(data)[1] <- "feature"
#' 
#' # Create a comparison table
#' comp_table <- data.frame(
#'    group = c("A", "A", "A", "A", "B", "B", "B", "B"),
#'    sample = c("T64552", "T64553", "T64554", "T64555", "T64546", "T64548", "T64549", "T64550")
#' )
#' 
#' # Create a Comparison object
#' comp <- Comparison$new(
#'     comparison_name = "A_over_B",
#'     group_order = c("B", "A"),
#'     comparison_table = comp_table
#' )
#' 
#' # Create a Pca object
#' pca_obj <- Pca$new(data, comp)
#' 
#' # Perform PCA
#' pca_obj$prcomp()
#' 
#' # Access PCA results
#' pca_obj$data                # View the input data
#' pca_obj$prcomp_results      # View the raw PCA results
#' pca_obj$prcomp_refined      # View the refined PCA results
#' 
#' # Create visualisations
#' scree_plot <- pca_obj$plot_scree()    # Generate a scree plot
#' scatter_plot <- pca_obj$plot_scatter() # Generate a scatter plot
#' 
#' # Print the scree plot
#' print(scree_plot)
#' 
#' # Print the scatter plot
#' print(scatter_plot)
#'
#' @export
Pca <- R6::R6Class(
    "Pca",
    public = list(
        data = NULL,
        comparison = NULL,
        prcomp_results = NULL, # prcomp class
        prcomp_refined = NULL,
        top_rotations = NULL,
        scatter = NULL,
        scree = NULL,
        #' @description
        #' Create a new Pca object
        #' @param data A data.table containing the input data for PCA. The first column must be named "feature".
        #' @param comparison An optional Comparison object for group comparisons
        initialize = function(data, comparison = NULL) {
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
        #' @description
        #' Perform Principal Component Analysis on the data
        #' @param ... Additional arguments passed to stats::prcomp
        #' @return NULL (results stored in Pca$prcomp_results, Pca$prcomp_refined)
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
        #' @description
        #' Print a summary of the PCA results
        #' @return NULL (prints to console)
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
        #' @description
        #' Generate a scree plot of the PCA results
        #' @param num_pc Number of principal components to include in the plot
        #' @return A ggplot2 object representing the scree plot
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
                theme_dereck_dark() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

            return(self$scree)
        },
        #' @description
        #' Generate a scatter plot of the first two principal components
        #' @param point_default_colour Default colour for points when no comparison is provided
        #' @param point_size Size of the points in the scatter plot
        #' @param point_alpha Alpha (transparency) of the points
        #' @param point_labels List of parameters for point labels
        #' @param top_contributors List of parameters for displaying top contributors
        #' @param title Title of the plot
        #' @param subtitle Subtitle of the plot
        #' @param caption Caption of the plot
        #' @return A ggplot2 object representing the scatter plot
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
            caption = if (top_contributors$show) stringr::str_interp('Top contributors to variance:\nPC1: ${paste0(stringr::str_trunc(names(self$top_rotations$PC1), top_contributors$truncate), collapse = ", ")}\nPC2: ${paste0(stringr::str_trunc(names(self$top_rotations$PC2), top_contributors$truncate), collapse = ", ")}') else NULL
        ) {
            private$validate_scatter_args(
                point_default_colour,
                point_size,
                point_alpha,
                point_labels,
                top_contributors
            )

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

            plot <- ggplot2::ggplot(
                    pc,
                    ggplot2::aes(
                        x = PC1,
                        y = PC2,
                        colour = highlight
                    )
                ) +
                ggplot2::geom_point(size = point_size, alpha = point_alpha)

            if (point_labels$show) {
                plot <- plot + ggrepel::geom_text_repel(
                    ggplot2::aes(label = sample),
                    size = point_labels$size,
                    max.overlaps = point_labels$max_overlaps,
                    alpha = point_labels$alpha,
                    fontface = point_labels$font_face
                )
            }

            plot <- plot +
                ggplot2::scale_colour_identity() +
                ggplot2::labs(
                    title = title,
                    subtitle = subtitle,
                    x = paste('PC1 - ', self$prcomp_refined$pct_var_explained[1], '%', sep = ''),
                    y = paste('PC2 - ', self$prcomp_refined$pct_var_explained[2], '%', sep = ''),
                    colour = if (!is.null(comparison)) "Group" else NA,
                    caption = caption
                ) +
                theme_dereck_dark() +
                ggplot2::theme(legend.position = "bottom")

            self$scatter <- plot
            return(plot)
        }
    ),
    private = list(
        #' Filter samples based on a comparison object
        filter_comparison_samples = function(data, comparison) {
            selected_samples <- colnames(data) %in% comparison$comparison_table$sample
            selected_samples[1] <- TRUE # keep GeneID/feature col

            return(data[, ..selected_samples][])
        },
        #' Refine PCA results for easier interpretation
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
        #' Prepare data for PCA by dropping non-numerical columns
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
        },
        validate_scatter_args = function(
            point_default_colour,
            point_size,
            point_alpha,
            point_labels,
            top_contributors
        ) {
            if (!is.character(point_default_colour) || length(point_default_colour) != 1) {
                stop("point_default_colour must be a single character string")
            }
            if (!is.numeric(point_size) || length(point_size) != 1) {
                stop("point_size must be a single numeric value")
            }
            if (!is.numeric(point_alpha) || length(point_alpha) != 1 || point_alpha < 0 || point_alpha > 1) {
                stop("point_alpha must be a single numeric value between 0 and 1")
            }
            if (!is.list(point_labels) || !all(c("show", "size", "max_overlaps", "alpha", "font_face") %in% names(point_labels))) {
                stop("point_labels must be a list with elements: show, size, max_overlaps, alpha, font_face")
            }
            if (!is.logical(point_labels$show) || length(point_labels$show) != 1) {
                stop("point_labels$show must be a single logical value")
            }
            if (!is.numeric(point_labels$size) || length(point_labels$size) != 1) {
                stop("point_labels$size must be a single numeric value")
            }
            if (!is.numeric(point_labels$max_overlaps) || length(point_labels$max_overlaps) != 1) {
                stop("point_labels$max_overlaps must be a single numeric value")
            }
            if (!is.numeric(point_labels$alpha) || length(point_labels$alpha) != 1 || point_labels$alpha < 0 || point_labels$alpha > 1) {
                stop("point_labels$alpha must be a single numeric value between 0 and 1")
            }
            if (!is.character(point_labels$font_face) || length(point_labels$font_face) != 1) {
                stop("point_labels$font_face must be a single character string")
            }
            if (!is.list(top_contributors) || !all(c("show", "truncate") %in% names(top_contributors))) {
                stop("top_contributors must be a list with elements: show, truncate")
            }
            if (!is.logical(top_contributors$show) || length(top_contributors$show) != 1) {
                stop("top_contributors$show must be a single logical value")
            }
            if (!is.numeric(top_contributors$truncate) || length(top_contributors$truncate) != 1) {
                stop("top_contributors$truncate must be a single numeric value")
            }
        }
    )
)
