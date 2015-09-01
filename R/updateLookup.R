

#' Update pair matrix using data.
#'
#' Optimization is restricted to +/- penalty * number of records with pairing.
#' Pairs are shuffled before optimization.
#' 
#' @param data data.frame with columns round, 
#' player1, player2, result, list1, list2, compare
#' @param pairlookup square matrix with rownames in list1 and colnames in list2.
#' @param pairs matrix with two colums
#' where each row is a pair of lists to optimize
#' @param penalty, single numeric, magnitude of maximum penalty available to pairs (default 30)
#' @param seed single numeric, reproducible randomization
#' @param player1 single character name of player one column (default "player1")
#' @param player2 single character name of player two column (default "player2")
#' @param round single character name of round column (default "round")
#' @param result single character name of result column (default "result")
#' @param compare single character name of comparison column (default "scorefrac")
#' @param list1 single character name of player one's list column (default "list1")
#' @param list2 single character name of player two's list column (default "list2")
#' @return matrix with rownames in list1 and colnames in list2.
#' @export
#' @examples
#' dat <- data.frame(round = rep(1:6, each = 2),
#'     player1 = rep(c("A", "B"), times = 2), 
#'     player2 = rep(c("B", "A"), times = 2),
#'     result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
#'     list1 = c("a", "b", "b", "a"), 
#'     list2 = c("b", "a", "a", "b"), 
#'     scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
#'     stringsAsFactors = FALSE)
#' updateLookup(data = dat)

updateLookup <- function(data, pairlookup = NULL, pairs = NULL, 
    penalty = 30, seed = NULL,
    compare = "scorefrac", round = "round", 
    player1 = "player1", player2 = "player2", 
    result = "result", list1 = "list1", list2 = "list2") {
    
    if (is.null(pairlookup)) { 
        
        pairlookup <- initializeLookup(data = data)
    }
    
    if (is.null(pairs)) { 
        
        pairs <- getPairs(data = data[c(list1, list2)]) 
    }
    
    # shuffle pairs before proceeding
    if (!is.null(seed)) {
        set.seed(seed)
    }
    pairs <- pairs[order(runif(n = nrow(pairs))), , drop = FALSE]
    
    for (p in seq_len(nrow(pairs))) {
        
        nrecords <- sum((data[[list1]] == pairs[p, 1] & 
                    data[[list2]] == pairs[p, 2]) |
                (data[[list2]] == pairs[p, 1] & 
                    data[[list1]] == pairs[p, 2]))
        
        vmin <- optimize(getNewStat, 
            interval = c(- penalty * nrecords^2, penalty * nrecords^2), 
            data = data, 
            pairlookup = pairlookup, pair = pairs[p, ], 
            compare = compare, round = round, 
            player1 = player1, player2 = player2, 
            result = result, list1 = list1, list2 = list2,
            tol = 0.1)
        
        pairlookup <- setMatrixVal(
            list1 = pairs[p, 1L], 
            list2 = pairs[p, 2L], 
            x = pairlookup, 
            val = vmin$minimum)
    }
    
    return(pairlookup)
}


#' Unique combinations paired in two records.
#'
#' Pairings of X vs X are not included.
#'
#' @param data matrix, list or data.frame with two columns
#' @return character matrix with two columns
#' @export
#' @examples 
#' dat1 <- data.frame(list1 = c("a", "b", "b"), list2 = c("b", "b", "a"))
#' getPairs(data = dat1)
#' dat2 <- data.frame(list1 = c("A", "A", "B"), list2 = c("C", "B", "D"))
#' getPairs(data = dat2)

getPairs <- function(data) {
    
    if (is.list(data) && !is.data.frame(data)) { 
        data <- as.data.frame(data, stringsAsFactors = FALSE) }
    
    if (ncol(data) != 2L) { stop("exactly two columns expected in data") }
    
    allPairs <- apply(X = data, MARGIN = 1L, FUN = sort)
    
    allPairs <- t(allPairs)
    
    allPairs <- allPairs[order(allPairs[, 1L], allPairs[, 2L]), , drop = FALSE]
    
    allPairs <- allPairs[!duplicated(allPairs), , drop = FALSE]
    
    allPairs <- allPairs[allPairs[, 1L] != allPairs[, 2L], , drop = FALSE]
    
    return(allPairs)
}


#' Create a square matrix for pairing strength scores
#'
#' Generate an empty pairlookup matrix with columns named in 
#' either list1 or list2.
#' @inheritParams updateLookup
#' @export
#' @examples
#' dat <- data.frame(list1 = c("A", "A", "B"), list2 = c("C", "B", "D"))
#' initializeLookup(data = dat)

initializeLookup <- function(data, list1 = "list1", list2 = "list2") {
    
    lists <- unique(unlist(data[c(list1, list2)]))
    lists <- sort(na.omit(lists))

    pairLookup <- matrix(0, 
        nrow = length(lists), 
        ncol = length(lists), 
        dimnames = list(lists, lists))
    
    return(pairLookup)
}

