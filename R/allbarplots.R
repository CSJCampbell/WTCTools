
#' @title Create Barplots for all Rows in a Matrix
#' @description
#' Creates a barplot or PNG file for each selected row 
#' of each matrix provided, where each horizontal bar is 
#' one column in that matrix.
#' 
#' @param lookups matrix or named list of matrices, 
#' each with symmetric row names and column names
#' @param lists character vector names from lookups
#' @param fileroot single character label to prepend to start of filenames,
#' perhaps including path, which will be constructed 
#' "<fileroot><listname><lookupname>.png".
#' If NULL, plots will be created in the windows device.
#' @param col single colour for bars
#' @param height single integer, height of png image to create (px)
#' @param width single integer, width of png image to create (px)
#' @param res single integer, resolution of png image to create (px/inch)
#' @param alpha single integer (0-255), transparency of error bars (default 50)
#' @return NULL
#' @export
#' @examples
#' pairs <- list("2013" = pairLookup13, 
#'     "2014" = pairLookup14, "2015" = pairLookup15)
#' allbarplots(lookups = pairs, 
#'     lists = khador)

allbarplots <- function(lookups, lists, 
    fileroot = NULL, col = "red", 
    height = 800, width = 800, res = 100, alpha = 50, ...) {
    if (is.matrix(lookups)) { lookups <- list(lookups) }
    if (!is.list(lookups)) { stop("lookups must be a list of matrices") }
    labs <- names(lookups)
    if (is.null(labs)) { labs <- seq_along(lookups) }
    if (alpha <= 1) { alpha <- alpha * 255 }
    col <- rgb(t(col2rgb(col)[1:3, ]), 
        alpha = alpha, maxColorValue = 255)
    
    op <- par(list("mar", "las"))
    for (lst in lists) {
        # total games played
        list2Nums <- getMatrixVal(
            x = attr(x = lookups[[length(lookups)]], which = "n"),
            list1 = lst)
        notZero <- list2Nums != 0
        # advantage
        list2Vals <- getMatrixVal(x = lookups[[length(lookups)]], 
            list1 = lst)[notZero]
        # list pairings from best to worst
        list2s <- rownames(lookups[[length(lookups)]])[notZero]
        list2s <- list2s[order(list2Vals, decreasing = FALSE)]
        # symmetric limit
        lims <- range(lookups[[length(lookups)]])
        lims <- max(abs(lims)) * c(-1, 1)
        # create plot for each list1 at each lookup
        for (pl in seq_along(lookups)) {
            if (!is.null(fileroot)) {
                png(paste0(fileroot, lst, labs[pl], ".png"), 
                    height = height, width = width, res = res)
            }
            par(mar = c(2, 7, 2, 1), las = 1)
            tbarplot(x = lookups[[pl]], 
                list1 = lst, 
                list2 = list2s, 
                pmain = labs[[pl]], 
                col = col, xlim = lims, ...)
            if (!is.null(fileroot)) {
                dev.off()
            }
        }
    }
    par(mar = op)
}


#' @title Horizontal Barplot
#' @param x matrix, with symmetric row names and column names
#' @param list1 single character, names from x
#' @param list2 character vector, names from x
#' @param pmain single character main title
#' @param penalty single numeric, step size in x
#' @param col single colour for bars
#' @param xlim length 2 numeric, range of x axis
#' @param bands numeric vector, number of band multiples
#' @return numeric vector of band positions invisibly.
#' @export
#' @examples
#' nms <- c("Butcher3", "Caine2", "Issyria", "Saeryn")
#' pl <- matrix(c(0:3, 1, 0:3, 2, 0:3, 1, 0), 
#'     nrow = 4, ncol = 4, dimnames = list(nms, nms))
#' attr(pl, "n") <- pl
#' tbarplot(x = pl, list1 = "Butcher3", list2 = nms[-1])

tbarplot <- function(x, list1, list2, 
    pmain = "", penalty = 10, 
    col = "#FF000022", xlim, bands = c(1, 3, 10), ...) {
    
    vec <- x[list1, list2]
    se <- sqrt((0.5 * 0.5) / attr(x = x, which = "n")[list1, list2])
    se[!is.finite(se)] <- NA_integer_
    if (missing(xlim)) { xlim <- range(vec) + 
        c(2 * max(bands, na.rm = TRUE) * penalty * max(se, na.rm = TRUE), 
        - 2 * max(bands, na.rm = TRUE) * penalty * max(se, na.rm = TRUE)) }
    # blank canvass
    pos <- barplot(vec, names.arg = "",
        horiz = TRUE, main = "",
        col = NA, border = NA,
        xlim = xlim, axes = FALSE, ...)
    
    
    # add transparent bars
    for (lev in bands) {
        tip <- 2 * lev * penalty * se
        root <- vec - lev * penalty * se
        barplot(tip,
            names.arg = "",
            horiz = TRUE, main = "",
            col = col, border = col,
            xlim = xlim, axes = FALSE, 
            offset = root,
            add = TRUE, 
            ...)
    }
    # add text
    barplot(vec, 
        horiz = TRUE, main = paste(list1, pmain),
        col = NA, 
        xlim = xlim, add = TRUE, ...)
    abline(v = 0, col = "grey")
    invisible(pos)
}
