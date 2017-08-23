
#' @title Create Calibration Data
#' @description Generates binned calibration curve for 
#' matchup data.
#' @inheritParams updateLookup
#' @return data.frame with columns PredictedWins and ProportionWins
#' @param binsize single numeric number of records to include in each bin
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
#' calibration(data = dat, pairlookup = pl)

calibration <- function(data, status = NULL, 
    pairlookup, binsize = 10, round = "round", 
    player1 = "player1", player2 = "player2", 
    result = "result", list1 = "list1", list2 = "list2") {
    
    if (missing(data)) { stop("data is missing") }
    
    nn <- ceiling(nrow(data) / binsize)
    if (nn < 4) { warning("too few bins, decrease binsize") }
    
    if (missing(pairlookup)) { stop("pairlookup is missing") }
    
    if (!is.matrix(pairlookup) || !(nrow(pairlookup) == ncol(pairlookup))) {
        stop("pairlookup must be a square matrix")
    }
    
    if (!all(rownames(pairlookup) %in% colnames(pairlookup))) {
        stop("rownames of pairlookup must be colnames of pairlookup")
    }
    data$pstephplayer1 <- getPred(data = data, status = status,
        pairlookup = pairlookup, round = round, 
        player1 = player1, player2 = player2, 
        result = result, list1 = list1, list2 = list2)
    
    qtl <- quantile(data$pstephplayer1, probs = (seq_len(nn + 1) - 1)/nn)
    data$p1winbin <- cut(data$pstephplayer1, 
        breaks = qtl, 
        labels = diff(qtl)*0.5 + qtl[-length(qtl)],
        include.lowest = TRUE)
    # dplyr syntax
    names(data)[names(data) == result] <- "result"
    # mean is proportion of 0 and 1
    res <- summarise(group_by(data, p1winbin), ProportionWins = mean(result))
    res <- transmute(res, 
        PredictedWins = as.numeric(as.character(p1winbin)), ProportionWins)
    res
}
