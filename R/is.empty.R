#' Check if an object is empty
#'
#' This function checks if an object is considered empty based on its class.
#'
#' @param x An R object
#' @return A logical value: TRUE if the object is empty, FALSE otherwise
#' @export
#' @examples
#' # Vector
#' vec <- numeric(0)
#' is.empty(vec)  # Returns TRUE
#' 
#' # List
#' lst <- list()
#' is.empty(lst)  # Returns TRUE
#' 
#' # Data frame
#' df <- data.frame()
#' is.empty(df)  # Returns TRUE
is.empty <- function(x) {
    UseMethod("is.empty")
}

#' Default method for is.empty
#'
#' This function is called when is.empty is applied to an object of a class
#' for which no specific method is defined.
#'
#' @param x An R object
#' @return This function always throws an error
#' @export
#' @examples
#' \dontrun{
#' # This will throw an error
#' is.empty(quote(x))
#' }
is.empty.default <- function(x) {
    stop("is.empty is not defined for objects of class ", class(x))
}

#' Check if a vector is empty
#'
#' This function checks if a vector has length zero.
#'
#' @param x A vector object
#' @return A logical value: TRUE if the vector is empty, FALSE otherwise
#' @export
#' @examples
#' vec1 <- numeric(0)
#' is.empty(vec1)  # Returns TRUE
#' 
#' vec2 <- c(1, 2, 3)
#' is.empty(vec2)  # Returns FALSE
is.empty.vector <- function(x) {
    return(length(x) == 0)
}

#' Check if a list is empty
#'
#' This function checks if a list has length zero.
#'
#' @param x A list object
#' @return A logical value: TRUE if the list is empty, FALSE otherwise
#' @export
#' @examples
#' lst1 <- list()
#' is.empty(lst1)  # Returns TRUE
#' 
#' lst2 <- list(a = 1, b = 2)
#' is.empty(lst2)  # Returns FALSE
is.empty.list <- function(x) {
    return(length(x) == 0)
}

#' Check if a matrix is empty
#'
#' This function checks if a matrix has zero rows or zero columns.
#'
#' @param x A matrix object
#' @return A logical value: TRUE if the matrix is empty, FALSE otherwise
#' @export
#' @examples
#' mat1 <- matrix(nrow = 0, ncol = 5)
#' is.empty(mat1)  # Returns TRUE
#' 
#' mat2 <- matrix(1:6, nrow = 2)
#' is.empty(mat2)  # Returns FALSE
is.empty.matrix <- function(x) {
    return(nrow(x) == 0 || ncol(x) == 0)
}

#' Check if a data.frame is empty
#'
#' This function checks if a data.frame has zero rows or zero columns.
#'
#' @param x A data.frame object
#' @return A logical value: TRUE if the data.frame is empty, FALSE otherwise
#' @export
#' @examples
#' df1 <- data.frame()
#' is.empty(df1)  # Returns TRUE
#' 
#' df2 <- data.frame(a = 1:3, b = letters[1:3])
#' is.empty(df2)  # Returns FALSE
is.empty.data.frame <- function(x) {
    return(nrow(x) == 0 || ncol(x) == 0)
}

#' Check if a factor is empty
#'
#' This function checks if a factor has length zero.
#'
#' @param x A factor object
#' @return A logical value: TRUE if the factor is empty, FALSE otherwise
#' @export
#' @examples
#' fct1 <- factor(levels = c("A", "B"))
#' is.empty(fct1)  # Returns TRUE
#' 
#' fct2 <- factor(c("A", "B", "A"))
#' is.empty(fct2)  # Returns FALSE
is.empty.factor <- function(x) {
    return(length(x) == 0)
}

#' Check if a character string is empty
#'
#' This function checks if a character string has zero characters.
#'
#' @param x A character string
#' @return A logical value: TRUE if the string is empty, FALSE otherwise
#' @export
#' @examples
#' str1 <- ""
#' is.empty(str1)  # Returns TRUE
#' 
#' str2 <- "Hello"
#' is.empty(str2)  # Returns FALSE
is.empty.character <- function(x) {
    return(nchar(x) == 0)
}
