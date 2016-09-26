
#' Get a value from a Matrix
#'
#' Extractor function for named matrix
#' 
#' @param x matrix with named rows and columns
#' @param list1 character vector name of rows in matrix
#' @param list2 character column name of rows in matrix
#' @return single numeric
#' @export
#' @examples
#' m1 <- matrix(1:9, nrow = 3, 
#'     dimnames = list(letters[1:3], letters[1:3])) / 10
#' getMatrixVal(x = m1, 
#'     list1 = "b", list2 = c("a", "b"))
#' getMatrixVal(x = m1, 
#'     list1 = c("b", "b", "c", "a"), 
#'     list2 = c("a", "c", "c", "b"))

getMatrixVal <- function(x, list1, list2) {
    if (!is.matrix(x)) { stop("x must be a matrix") }
    if (ncol(x) != nrow(x)) {
        stop("x must have the same number of rows and columns") }
    if (missing(list1)) { list1 <- rownames(x) }
    if (missing(list2)) { list2 <- colnames(x) }
    if (length(list1) != length(list2) && 
        !(length(list1) == 1L || 
        length(list2) == 1L)) {
        stop("list1 and list2 must be the same length")
    }
    if (length(list1) == 1L) { 
        list1 <- rep(list1, times = length(list2))
    }
    if (length(list2) == 1L) { 
        list2 <- rep(list2, times = length(list1))
    }
    isLs1inx <- list1 %in% rownames(x)
    isLs2inx <- list2 %in% rownames(x)
    if (!all(isLs1inx & isLs2inx)) {
        missingLs <- unique(rownames(x)[!isLs1inx || !isLs2inx])
        stop("names specified in list1 or list2 not present in x:\n", 
            paste(missingLs, collapse = ", "))
    }
    val <- numeric(max(length(list1), length(list2)))
    for (i in seq_along(val)) {
        val[i] <- x[list1[i], list2[i]]
    }
    return(val)
}


#' Set a value in a Matrix
#'
#' Updater function for named matrix
#' 
#' @param x named matrix
#' @param list1 single character name of player 1 list
#' @param list2 single character name of player 2 list
#' @param val single numeric value to set
#' @param type single character specify how opposite pair should be entered: \itemize{
#'     \item neg negative
#'     \item same equal to pair
#'     \item 0-1 as 1 - pair (since pair is fraction)
#' }
#' negative or save sign? (default TRUE)
#' @return matrix
#' @export
#' @examples
#' m1 <- matrix(0, nrow = 3, ncol = 3, 
#'     dimnames = list(letters[1:3], letters[1:3])) / 10
#' setMatrixVal(list1 = "b", list2 = "a", x = m1, val = -2)
#' setMatrixVal(list1 = c("a", "b", "c"), 
#'     list2 = c("b", "c", "a"), x = m1, val = 1:3)

setMatrixVal <- function(x, list1, list2, val, 
    type = c("neg", "same", "0-1")) {
    if (missing(x)) { stop("x is missing") }
    if (missing(val)) { stop("val is missing") }
    if (length(list1) != length(list2)) { 
        stop("list1 and list2 must be the same length")
    }
    if (length(list1) != length(val) && length(val) != 1L) { 
        stop("list1 and val must be the same length") }
    if (length(val) == 1L) { 
        val <- rep(val, times = length(list1))
    }
    type <- match.arg(type)
    val2 <- switch(type,
        "neg" = -val,
        "same" = val,
        "0-1" = 1 - val
    )
    for (i in seq_along(val)) {
        x[list1[i], list2[i]] <- val[i]
        x[list2[i], list1[i]] <- val2[i]
    }
    return(x)
}
