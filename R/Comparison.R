#' Comparison Class
#'
#' An R6 class that represents a comparison between two groups of samples. 
#' This class contains the comparison name, the group order, and the comparison table.
#'
#' @field comparison_name Character. The name of the comparison.
#' @field group_order Character vector. The order of groups for the comparison, with length 2. The first element will be treated as the "control" group, and the second as the "test" group.
#' @field comparison_table A data.table that contains the group and sample information for the comparison.
#'
#' @description
#' The `Comparison` class represents a comparison between two groups of samples. It includes methods to initialize, print, and validate the data.
#'
#' @section Initialisation:
#' ```
#' Comparison$new(
#'   comparison_name = "My comparison",
#'   group_order = c("Control group", "Test group"),
#'   comparison_table = data.table::data.table(
#'        group = c("Control group", "Control group", "Test group", "Test group"),
#'        sample = c("Sample1", "Sample2", "Sample3", "Sample4")
#'    )
#' )
#' ```
#' @param comparison_name A character string representing the name of the comparison. Must be of length 1 and not exceed 100 characters.
#' @param group_order A character vector specifying the order of groups for the comparison. Must be of length 2.
#' @param comparison_table A data.table that contains group and sample information for the comparison. This table is crucial for tracking which samples belong to which clinical group. It is used to distinguish samples for the purposes of experimental analysis. It should have two columns, named "group" and "sample". The "group" column identifies the clinical group to which each sample belongs, and the "sample" column lists the names/IDs of the samples. The groups in this table should match the names specified in `group_order`.
#'
#' @section Usage:
#' ```
#' comparison <- Comparison$new(
#'   comparison_name = "My comparison",
#'   group_order = c("Control group", "Test group"),
#'   comparison_table = data.table::data.table(
#'        group = c("Control group", "Control group", "Test group", "Test group"),
#'        sample = c("Sample1", "Sample2", "Sample3", "Sample4")
#'    )
#' )
#' comparison$print()  # Print the Comparison object
#' ```
#'
#' @export
Comparison <- R6::R6Class(
    "Comparison",
    public = list(
        comparison_name = NA_character_,
        group_order = c(NA_character_, NA_character_),
        comparison_table = data.table::data.table(),
        initialize = function(
            comparison_name,
            group_order,
            comparison_table
        ) {
            if (
                missing(comparison_name) ||
                missing(group_order) ||
                missing(comparison_table)
            ) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: missing arguments; "comparison_name", "group_order", "comparison_table" must be provided.'))
            }

            data.table::setDT(comparison_table)

            self$comparison_name <- comparison_name
            self$group_order <- group_order
            self$comparison_table <- comparison_table

            map <- self$group_order %in% self$comparison_table$group
            if (!any(map)) {
                rlang::abort(stringr::str_interp('${self$group_order}: group_order lists comparisons not in the comparison_table; these were not found: ${collapse(self$group_order[!map])}'))
            }

            # filter comparison table for only those groups listed
            self$comparison_table <- self$comparison_table[
                group %in% self$group_order,
            ]

            data.table::setorderv(self$comparison_table, c("group", "sample"))

            derived_groups <- paste(c("control - ", self$group_order[1], ", test - ", self$group_order[2]), collapse = "")
            message(stringr::str_interp('${self$comparison_name}: deriving condition "control", "test" from group_order argument: ${derived_groups}'))

            self$comparison_table$condition <- "test"
            # TODO: revise to use data.table
            self$comparison_table[
                self$comparison_table$group == self$group_order[1],
                "condition"
            ] <- "control"

            # convert to factor
            self$comparison_table[
                , condition := factor(condition, levels = c("control", "test"))
            ]

            private$validate()
        },
        print = function() {
            cat("Comparison R6 object\n")
            cat("-----------------\n")
            cat("Comparison Name: ", self$comparison_name, "\n")
            cat("Group Order: ", paste(self$group_order, collapse = ", "), "\n")
            cat("Comparison Table:\n")
            cat(printCapture(self$comparison_table), "\n")
        }
    ),
    private = list(
        validate = function() {
            # print out comparison name everytime so in live code know which caused error
            if (length(self$comparison_name) != 1) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: "comparison_name" must be of type character length 1.'))
            }

            if (nchar(self$comparison_name) > 100) {
                rlang::warn(stringr::str_interp('${self$comparison_name}: "comparison_name" might be too long for practical use.'))
            }

            if (length(self$group_order) != 2) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: "group_order" must be of type character length 2; groups received: ${collapse(self$group_order)}'))
            }

            not_found <- !self$group_order %in% self$comparison_table$group
            if (any(not_found)) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: "group_order" lists groups not found in the "comparison_table"; missing: ${collapse(self$group_order[not_found])}'))
            }

            if (ncol(self$comparison_table) != 3) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: "comparison_table" must have two columns.'))
            }

            if (any(!c("group", "sample") %in% colnames(self$comparison_table))) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: "comparison_table" must have colnames: "group", "sample".'))
            }

            if (length(unique(self$comparison_table$group)) != length(unique(self$group_order))) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: "comparison_table" does not list the same number of unique groups as "group_order": ${collapse(unique(self$comparison_table$group))}'))
            }

            dup_map <- duplicated(self$comparison_table$sample)
            if (any(dup_map)) {
                rlang::abort(stringr::str_interp('${self$comparison_name}: slot "comparison_name" lists duplicate samples; received duplicates: ${collapse(self$comparison_table$sample[dup_map])}'))
            }
        }
    )
)
# Comparison$new(
#     comparison_name = "yeet",
#     group_order = c("ctrl", "test"),
#     comparison_table = data.table::data.table(
#         group = c("ctrl", "ctrl", "test", "test"),
#         sample = c("s1", "s2", "s3", "s4")
#      )
# )