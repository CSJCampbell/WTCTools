
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

