
#' @title Create Barplots for all Rows in a Matrix
#'
#' @description Creates a barplot or PNG file for each selected row 
#' of each matrix provided, where each horizontal bar is 
#' one column in that matrix.
#' 
#' @param lookups matrix or named list of matrices, 
#' each with symmetric row names and column names
#' @param lists character vector names from lookups
#' @param nmin single integer, minimum number of games for bar to be plotted (default 3L)
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
#' @importFrom grDevices col2rgb dev.off png rgb
#' @importFrom graphics abline barplot par
#' @importFrom methods is
#' @examples
#' wtc2014 <- na.omit(wtc2014)
#' wtc2014$scorefracCP <- wtc2014$CP1 / (wtc2014$CP1 + wtc2014$CP2)
#' ## if player 2 scored 0
#' wtc2014$scorefracCP[wtc2014$CP2 == 0] <- 0.5 + wtc2014$CP1[wtc2014$CP2 == 0] / (5 * 2)
#' ## if player 1 scored 0
#' wtc2014$scorefracCP[wtc2014$CP1 == 0] <- 0.5 - wtc2014$CP2[wtc2014$CP1 == 0] / (5 * 2)
#' ## fractional army points (approximately)
#' wtc2014$AP1 <- wtc2014$AP1 / 61
#' wtc2014$AP2 <- wtc2014$AP2 / 61
#' ## ratio of score
#' wtc2014$scorefracAP <- wtc2014$AP1 / (wtc2014$AP1 + wtc2014$AP2)
#' ## if player 2 scored 0
#' wtc2014$scorefracAP[wtc2014$AP2 == 0] <- 0.5 + wtc2014$AP1[wtc2014$AP2 == 0] / 2
#' ## if player 1 scored 0
#' wtc2014$scorefracAP[wtc2014$AP1 == 0] <- 0.5 - wtc2014$AP2[wtc2014$AP1 == 0] / 2
#' ## combine the scores
#' wtc2014$scorefrac <- 0.5 * wtc2014$scorefracCP + 0.5 * wtc2014$scorefracAP
#' wtc2014$scorefrac[wtc2014$scorefrac > 1] <- 1
#' \dontrun{
#' pairLookup14 <- updateLookup(data = wtc2014, 
#'     pairlookup = initializeLookup(data = unique(c(wtc2014$list1, wtc2014$list2))), 
#'     penalty = 10,
#'     result = "TP")
#' khador <- c("Butcher", "Butcher2", "Butcher3", "Harkevich", "Irusk", "Irusk2", 
#'     "Karchev", "Old Witch", "Sorscha", "Sorscha2", "Strakhov", "Vlad", "Vlad2", "Vlad3", 
#'     "Zerkova2")
#' allbarplots(lookups = pairLookup14, 
#'     lists = khador)
#' }

allbarplots <- function(lookups, lists, nmin = 3L,
    fileroot = NULL, col = "red", 
    height = 800, width = 800, res = 100, alpha = 50, ...) {
    if (is.matrix(lookups)) { lookups <- list(lookups) }
    if (!is.list(lookups)) { stop("lookups must be a list of matrices") }
    labs <- names(lookups)
    if (is.null(labs)) { labs <- seq_along(lookups) }
    if (alpha <= 1) { alpha <- alpha * 255 }
    col <- rgb(t(col2rgb(col)[1:3, ]), 
        alpha = alpha, maxColorValue = 255)
    if (missing(lists)) {
        listls <- lapply(X = lookups, FUN = rownames)
        lists <- listls[[1]]
        for (i in seq_len(length(listls) - 1L)) {
            if (!all(lists %in% listls[[i + 1L]])) {
                warning("not all lists in first lookup present in lookup ", i)
            }
        }
    }
    for (lst in lists) {
        # total games played
        list2Nums <- getMatrixVal(
            x = attr(x = lookups[[length(lookups)]], which = "n"),
            list1 = lst)
        notZero <- list2Nums >= nmin
        if (sum(notZero) > 1L) {
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
    }
}


#' @title Horizontal Barplot
#' 
#' @description Create barplots for a row of a pairlookup matrix.
#' 
#' @param x matrix, with symmetric row names and column names
#' @param list1 single character, names from x
#' @param list2 character vector, names from x
#' @param pmain single character main title
#' @param penalty single numeric, step size in x (default 10)
#' @param nmin single integer, minimum number of games for bar to be plotted (default 3L)
#' @param col single colour for bars
#' @param xlim length 2 numeric, range of x axis
#' @param bands numeric vector, number of band multiples (default c(1, 3, 10))
#' @return numeric vector of band positions invisibly.
#' @export
#' @examples
#' pl14 <- structure(c(4.80673409037624, 21.3865173349445, -5.17793802916065, 
#'         45.9138959155639, 14.5926699048978, -21.3865173349445, -33.4375895685107), 
#'     .Dim = c(1L, 7L), 
#'     .Dimnames = list("Butcher3", c("Caine2", "Deneghra", "Haley", "Haley2", 
#'         "Issyria", "Kreoss3", "Saeryn")))
#' attr(x = pl14, which = "n") <- structure(c(3, 2, 2, 4, 3, 2, 3), 
#'     .Dim = c(1L, 7L), 
#'     .Dimnames = list("Butcher3", c("Caine2", "Deneghra", "Haley", "Haley2", 
#'         "Issyria", "Kreoss3", "Saeryn")))
#' nm <- colnames(pl14)[order(pl14["Butcher3", ])]
#' tbarplot(x = pl14, list1 = "Butcher3", list2 = nm)

tbarplot <- function(x, list1, list2, 
    pmain = "", penalty = 10, nmin = 3L,
    col = "#FF000022", xlim, bands = c(1, 3, 10), 
    mar = c(2, 7, 2, 1), las = 1, ...) {
    if (!is.matrix(x)) { 
        stop("x must be a matrix, but was a ", class(x))
    }
    if (length(list1) != 1) { 
        stop("list1 must be length 1, but was length ", length(list1))
    }
    if (!list1 %in% rownames(x)) { 
        stop("list1 must be named in rows of x")
    }
    if (!all(list2 %in% colnames(x))) {
        stop("list2 must be named in columns of x")
    }
    vec <- x[list1, list2]
    n <- attr(x = x, which = "n")[list1, list2]
    if (all(nmin > n)) {
        stop("nmin was ", nmin, " but largest n was ", max(n, na.rm = TRUE))
    }
    # trim out low count bars
    vec <- vec[n >= nmin]
    n <- n[n >= nmin]
    # error bars
    se <- sqrt((0.5 * 0.5) / n)
    se[!is.finite(se)] <- NA_integer_
    if (missing(xlim)) { xlim <- range(range(vec) + 
        c(-2 * max(bands, na.rm = TRUE) * penalty * max(se, na.rm = TRUE), 
        2 * max(bands, na.rm = TRUE) * penalty * max(se, na.rm = TRUE))) }
    
    op <- par(list("mar", "las"))
    par(mar = mar, las = las)
    on.exit(par(mar = op))
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
        names.arg = paste(names(vec), paste0("(n = ", n, ")")),
        horiz = TRUE, main = paste(list1, pmain),
        col = NA, 
        xlim = xlim, add = TRUE, ...)
    abline(v = 0, col = "grey")
    invisible(pos)
}
