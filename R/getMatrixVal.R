
#' Get a value from a Matrix
#'
#' Extractor function for named matrix
#' 
#' @param list1 character vector name of rows in matrix
#' @param list2 character column name of rows in matrix
#' @param x matrix with named rows and column
#' @return single numeric
#' @export
#' @examples
#' m1 <- matrix(1:9, nrow = 3, dimnames = list(letters[1:3], letters[1:3])) / 10
#' getMatrixVal(list1 = "b", list2 = "a", x = m1)
#' getMatrixVal(list1 = c("b", "b", "c", "a"), list2 = c("a", "c", "c", "b"), x = m1)

getMatrixVal <- function(list1, list2, x) {
    if (length(list1) != length(list2)) { stop("list1 and list2 must be the same length") }
    val <- numeric(length(list1))
    for (i in seq_along(val)) { val[i] <- x[list1[i], list2[i]] }
    return(val)
}


#' Set a value in a Matrix
#'
#' Updater function for named matrix
#' 
#' @param list1 name
#' @param list2 name
#' @param x matrix
#' @param val single numeric
#' @return matrix
#' @export
#' @examples
#' m1 <- matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3])) / 10
#' setMatrixVal(list1 = "b", list2 = "a", x = m1, val = -2)
#' setMatrixVal(list1 = c("a", "b", "c"), list2 = c("b", "c", "a"), x = m1, val = 1:3)

setMatrixVal <- function(list1, list2, x, val) { 
    if (length(list1) != length(list2)) { stop("list1 and list2 must be the same length") }
    if (length(list1) != length(val) && length(val) != 1L) { stop("list1 and val must be the same length") }
    if (length(val) == 1L) { val <- rep(val, times = length(list1)) }
    for (i in seq_along(val)) {
        x[list1[i], list2[i]] <- val[i]
        x[list2[i], list1[i]] <- -val[i]
    }
    return(x)
}
