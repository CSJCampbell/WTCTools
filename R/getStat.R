

#' Get statistic from difference between predicted value and scorefrac
#' 
#' Use additional information about closeness of game 
#' to find goodness of prediction.
#'
#' @inheritParams updateLookup
#' @export
#' @examples
#' pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
#'     dimnames = list(c("a", "b"), c("a", "b")))
#' dat <- data.frame(round = rep(1:6, each = 2),
#'     player1 = rep(c("A", "B"), times = 2), 
#'     player2 = rep(c("B", "A"), times = 2),
#'     result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
#'     list1 = c("a", "b", "b", "a"), 
#'     list2 = c("b", "a", "a", "b"), 
#'     scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
#'     stringsAsFactors = FALSE)
#' getStat(data = dat, pairlookup = pl, result = "result")

getStat <- function(data, pairlookup, 
    compare = "scorefrac", round = "round", 
    player1 = "player1", player2 = "player2", 
    result = "result", list1 = "list1", list2 = "list2") {
    
    if (missing(data)) { stop("data is missing") }
    
    if (missing(pairlookup)) { stop("pairlookup is missing") }
    
    if (!is.matrix(pairlookup) || !(nrow(pairlookup) == ncol(pairlookup))) {
        stop("pairlookup must be a square matrix")
    }
    
    if (!all(rownames(pairlookup) %in% colnames(pairlookup))) {
        stop("rownames of pairlookup must be colnames of pairlookup")
    }
    
    pstephplayer1 <- getPred(data = data, 
        pairlookup = pairlookup, round = round, 
        player1 = player1, player2 = player2, 
        result = result, list1 = list1, list2 = list2)
    
    stat <- sum(((data[[compare]] - 0.5) - (pstephplayer1 - 0.5))^2)
    
    return(stat)
}


#' Set Matrix Value, then Get Statistic
#' 
#' Minimization tool for setting pairlookup, 
#' then getting statistic from difference between 
#' predicted value and scorefrac using \code{\{link{getStat}}.
#' 
#' @param val numeric value for pairlookup at pair
#' @param data data.frame with columns round, 
#' player1, player2, result, list1, list2, scorefrac
#' @param pairlookup matrix with names in list1
#' @param pair length two vector with names in list1
#' @inheritParams updateLookup
#' @return single numeric optimized difference between scorefrac 
#' and p-steph for a player, given a list pair lookup
#' @export
#' @examples
#' \dontrun{
#' getNewStat(val = 10, data = wtct, 
#'     pairlookup = look, pair = c("Absylonia2", "Baldur2"))
#' # [1] 87.14286
#' }
#' pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
#'     dimnames = list(c("a", "b"), c("a", "b")))
#' dat <- data.frame(round = rep(1:6, each = 2),
#'     player1 = rep(c("A", "B"), times = 2), 
#'     player2 = rep(c("B", "A"), times = 2),
#'     result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
#'     list1 = c("a", "b", "b", "a"), 
#'     list2 = c("b", "a", "a", "b"), 
#'     scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
#'     stringsAsFactors = FALSE)
#' getNewStat(val = 60, data = dat, pairlookup = pl, 
#'     pair = c("a", "b"))

getNewStat <- function(val, data, pairlookup, pair, 
    compare = "scorefrac", round = "round", 
    player1 = "player1", player2 = "player2", 
    result = "result", list1 = "list1", list2 = "list2") {
    
    pairlookup <- setMatrixVal(list1 = pair[1], list2 = pair[2], 
        x = pairlookup, val = val)
    
    stat <- getStat(data = data, pairlookup = pairlookup, 
        compare = compare, round = round, 
        player1 = player1, player2 = player2, 
        result = result, list1 = list1, list2 = list2)
    
    return(stat)
}
