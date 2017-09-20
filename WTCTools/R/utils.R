
#' Create a pair reversing index
#' 
#' Provide an even integer. Create an index vector to reverse paired values in 
#' a vector. 
#' 
#' @param n single even integer
#' @return integer vector of length n
#' @export
#' @examples
#' ind(n = 4)
#' ## [1] 2 1 4 3
#' ind(n = 6)
#' ## [1] 2 1 4 3 6 5

ind <- function(n) {
    if (n %% 2 != 0) { warning("could not match all records") }
    out <- integer(n)
    alt <- seq.int(from = 1, to = floor(n / 2)) * 2
    out[alt - 1] <- alt
    out[alt] <- alt - 1
    return(out)
}

#' @title Print names that are similar
#' @description use \code{adist} to find matrices of distances
#' between names. First letter is not usually incorrect, 
#' so search alphabetically.
#' @param s character vector
#' @param dist integer specifying how close records should be
#' @return NULL
#' @importFrom stringi stri_trans_general
#' @export
#' @examples
#' showSimilarNames(c("Minnie", "Millie", "Ernestine"))

showSimilarNames <- function(s, dist = 4L) {
    pn <- stri_trans_general(str = unique(s), id = "latin-ascii")
    counter <- 1L
    out <- matrix(NA_character_, nrow = length(s), ncol = 2)
    for (let in letters) {
        pnlet <- grep(pattern = paste0("^", let), 
            x = casefold(pn), value = TRUE)
        if (length(pnlet) > 0) {
            dmatlet <- adist(x = pnlet)
            # don't show reverse pair or self-pair
            dmatlet <- dmatlet * lower.tri(dmatlet)
            pnlet <- pn[casefold(pn) %in% pnlet]
            for (rowlet in seq_len(nrow(dmatlet))) {
                x <- dmatlet[rowlet, ]
                if (any(x[x != 0] < dist)) {
                    for (hits in pnlet[x < dist & x > 0]) {
                        out[counter, ] <- c(pnlet[rowlet], hits)
                        counter <- counter + 1L
                    }
                }
            }
        }
    }
    out[apply(out, MARGIN = 1, FUN = function(x) !all(is.na(x))), , 
        drop = FALSE]
}
