

#' Get player one's probability of winning
#'
#' Calculate expected performance of a matchup for a player, given a list matchup.
#' pairlookup contains gamma correction for game between player one and player two.
#' Positive values favour player one, while negative values favour player two.
#' get player ratings
#'
#' @inheritParams updateLookup
#' @return single numeric optimized difference between scorefrac 
#' and p-steph for a player, given a list pair lookup
#' @param compare single character, name of column to compare
#' @import PlayerRatings
#' @export
#' @examples
#' pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
#'     dimnames = list(c("a", "b"), c("a", "b")))
#' dat <- data.frame(round = rep(1:6, each = 2),
#'     player1 = rep(c("A", "B"), times = 2), 
#'     player2 = rep(c("B", "A"), times = 2),
#'     result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
#'     list1 = c("a", "b", "b", "a"), 
#'     list2 = c("b", "a", "a", "b"), stringsAsFactors = FALSE)
#' getPred(data = dat, pairlookup = pl, result = "result")

getPred <- function(data, status = NULL, pairlookup, round = "round", 
    player1 = "player1", player2 = "player2", 
    result = "result", list1 = "list1", list2 = "list2") {
    
    gammaVar <- getMatrixVal(
        x = pairlookup, 
        list1 = data[[list1]], 
        list2 = data[[list2]])
    
    # performance of player given list
    rating_player <- steph(x = data[, c(round, player1, player2, result)], 
        status = status,
        gamma = gammaVar)
    
    pstephplayer1 <- predict(object = rating_player, 
        data[, c(round, player1, player2)], 
        tng = 0, gamma = gammaVar)

    return(pstephplayer1)
}
