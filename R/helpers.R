#' Collapse a vector into a single string
#'
#' This function collapses a vector into a single string, with elements separated by a comma and a space.
#'
#' @param x A vector of elements to collapse.
#' @return A single string with all elements from the vector separated by a comma and a space.
#' @examples
#' collapse(c("a", "b", "c"))
#' # Returns: "a, b, c"
collapse <- function(x) {
    return(paste(x, collapse = ", "))
}

#' Print and capture the output of an object
#'
#' This function prints an object and captures the output as a string.
#' 
#' This is useful for printing data.frames to log files etc.
#'
#' @param obj The object to print and capture the output from.
#' @return A string representing the printed output of the object.
#' @export
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' printCapture(df)
#' # Returns a string representation of the data frame
printCapture <- function(obj) {
    return(paste(utils::capture.output(print(as.data.frame(obj))), collapse = "\n"))
}

#' Find coordinates of a specific value in an object
#'
#' This function finds the coordinates (row and column indices) of a specific value in an object.
#' If no value is provided, it finds the coordinates of NA values.
#'
#' @param obj The object to find the value in. Should be a matrix or data frame.
#' @param value The value to find in the object. Default is NA.
#' @return A data frame with the column and row indices of the value in the object.
#' @export
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat[2, 2] <- NA
#' valueCoordinates(mat)
#' # Returns a data frame with the coordinates of NA
#' valueCoordinates(mat, 5)
#' # Returns a data frame with the coordinates of the value 5
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

#' Convert a data.table to a data.frame
#'
#' This function converts a data.table to a data.frame, optionally setting row names from a specified column.
#'
#' @param x A data.table object to convert.
#' @param id.col The name of the column to use as row names. If NULL, no row names are set. Default is NULL.
#' @param drop.id.col Logical. If TRUE and id.col is specified, the id column is removed after setting row names. Default is TRUE.
#' @return A data.frame converted from the input data.table.
#' @export
#' @examples
#' library(data.table)
#' dt <- data.table(id = letters[1:3], value = 1:3)
#' to.data.frame(dt, id.col = "id")
#' # Returns a data.frame with row names set to "a", "b", "c" and "id" column removed
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
