
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

#' @title Convert Accented Characters to Plain
#' @description uses \code{chartr} to transpose characters.
#' https://stackoverflow.com/questions/17517319/r-replacing-foreign-characters-in-a-string
#' @param s character vector
#' return character vector
#' @export
#' @examples
#' toPlain("Štepan Slavik")

toPlain <- function(s) {
    # 1 character substitutions
    old1 <- "ÂÅÃŠšžþàáâãäåçèéêëìíîïðñòóôõöùúûüý"
    new1 <- "AAASszyaaaaaaceeeeiiiidnooooouuuuy"
    s1 <- chartr(old1, new1, s)

    # 2 character substitutions
    old2 <- c("œ", "ß", "æ", "ø")
    new2 <- c("oe", "ss", "ae", "oe")
    s2 <- s1
    for (i in seq_along(old2)) {
        s2 <- gsub(pattern = old2[i], 
            replacement = new2[i], x = s2, fixed = TRUE)
    }
    s2
}

#' @title Print names that are similar
#' @examples
#' showSimilarNames(c("Minnie", "Millie", "Ernestine"))

showSimilarNames <- function(s, dist = 4L) {
    pn <- stri_trans_general(str = s, id = "latin-ascii")
    
    for (let in letters) {
        pnlet <- grep(pattern = paste0("^", let), 
            x = casefold(pn), value = TRUE)
        if (length(pnlet) > 0) {
            dmatlet <- adist(x = pnlet)
            dmatlet <- dmatlet * lower.tri(dmatlet)
            pnlet <- pn[casefold(pn) %in% pnlet]
            for (rowlet in seq_len(nrow(dmatlet))) {
                x <- dmatlet[rowlet, ]
                if (any(x[x != 0] < dist)) {
                    for (hits in pnlet[x < dist & x > 0]) {
                        message(pnlet[rowlet], "  :  ", hits)
                    }
                }
            }
        }
    }
}
