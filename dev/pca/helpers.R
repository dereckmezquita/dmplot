#' Collapse a vector into a single string
#'
#' This function collapses a vector into a single string, with elements separated by a comma and a space.
#'
#' @param x A vector of elements to collapse.
#' @return A single string with all elements from the vector separated by a comma and a space.
#' @export
collapse <- function(x) return(paste(x, collapse = ", "))

#' Print and capture the output of an object
#'
#' This function prints an object and captures the output as a string.
#'
#' @param obj The object to print and capture the output from.
#' @return A string representing the printed output of the object.
#' @export
printCapture <- function(obj) return(paste(utils::capture.output(print(as.data.frame(obj))), collapse = "\n"))

#' Find coordinates of a specific value in an object
#'
#' This function finds the coordinates (row and column indices) of a specific value in an object.
#' If no value is provided, it finds the coordinates of NA values.
#'
#' @param obj The object to find the value in.
#' @param value The value to find in the object. Default is NA.
#' @return A data frame with the column and row indices of the value in the object.
#' @export
valueCoordinates <- function(obj, value = NA) {
    if(is.na(value)) {
        truths <- is.na(obj)
    } else {
        truths <- obj == value
    }

    r <- apply(truths, 2, function(x) {
        if(any(which(x))) {
            return(unname(which(x)))
        } else {
            return(NA)
        }
    })

    c <- apply(truths, 1, function(y) {
        if(any(which(y))) {
            return(unname(which(y)))
        } else {
            return(NA)
        }
    })

    r <- unname(r[!is.na(r)])
    c <- unname(c[!is.na(c)])

    return(data.frame(column = unlist(c), row = unlist(r)))
}

#' @export
to.data.frame <- function(x, id.col = NULL, drop.id.col = TRUE) {
    ans <- data.table::copy(x)

    if(!is.null(id.col)) {
        if(!id.col %in% colnames(ans)) {
            rlang::abort(stringr::str_interp('Column "${id.col}" not found.'))
        }

        data.table::setDF(ans, rownames = ans[, get(id.col)])

        if(drop.id.col) {
            ans[, id.col] = NULL
        }
    } else {
        data.table::setDF(ans)
    }

    return(ans)
}