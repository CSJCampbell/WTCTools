
#' Pair Teams on the Same Score
#' 
#' Returns a vector where each team in an odd position plays the
#' team in the following even position.
#' 
#' @param results NULL or data frame with a row named for each team 
#' and with columns:\itemize{
#'     \item Total.Games.Won numeric total number of games won
#'     \item Matches.Won numeric total number of matches won
#'     \item Opponent.Round.X one or more columns naming the opponent in each round
#' }
#' @param maxit single integer number of times to repeat
#' @return character vector length number of rows results
#' @export
#' @examples
#' eg <- data.frame(Total.Games.Won = c(5, 4, 4, 3), 
#'     Matches.Won = c(1, 1, 1, 1), 
#'     Opponent.Round.1 = c("e", "c", "b", "f"), 
#'     Opponent.Round.2 = c("b", "a", "f", "e"), 
#'     stringsAsFactors = FALSE)
#' rownames(eg) <- letters[1:4]
#' makeMatchup(results = eg)

makeMatchup <- function(results, maxit = 10L) {
    # check results
    if (!is.data.frame(results)) {
        stop("results must be NULL or a data.frame")
    }
    expectCols <- c("Total.Games.Won", "Matches.Won")
    isMissingCols <- !expectCols %in% colnames(results)
    if (any(isMissingCols)) {
        stop("missing columns in results: ", 
            paste(expectCols[isMissingCols], collapse = ", "))
    }
    ng <- nrow(results)
    if (ng %% 2 != 0) {
        warning("results should have an even number of rows") }
    # previous rounds
    oppRndIndex <- grep(pattern = "Opponent\\.Round", 
            x = colnames(results))
    # not expected
    if (length(oppRndIndex) < 1L) {
        warning("no columns with pattern Opponent.Round in results")
    }
    # check whether teams were paired together more than once in results
    invalidInput <- apply(X = results[, oppRndIndex, drop = FALSE], 
        MARGIN = 1L, FUN = function(x) { any(duplicated(x)) })
    if (any(invalidInput)) { 
        warning("duplicated results for ", sum(invalidInput), " teams") }
    
    # check whether teams were assigned more than once in a round in results
    invalidInput <- sapply(X = results[, oppRndIndex, drop = FALSE], 
        FUN = function(x) { any(duplicated(x)) })
    if (any(invalidInput)) { 
        stop("duplicated results for ", sum(invalidInput), " rounds") }
    
    # how many of this group have already been paired
    prev <- apply(X = results[, oppRndIndex, drop = FALSE], 
        MARGIN = 1L, FUN = function(x, teams) { sum(x %in% teams) }, 
        teams = rownames(results))
    allowRepeats <- FALSE
    goAgain <- TRUE
    if (any(prev >= (ng - 1L))) {
        warning("could not pair all teams with new opponents")
        allowRepeats <- TRUE
    }
    nit <- 0L
    matchPairs <- rep(NA_character_, times = ng)
    # iterate to try to avoid repeat pairing
    while (any(is.na(matchPairs)) && goAgain) {
        if (allowRepeats) { goAgain <- FALSE }
        # start with the most paired teams 
        # to reduce the likelihood of leaving a team with
        # no valid matchup
        ord <- names(sort(prev, decreasing = TRUE))
        matchPairs <- rep(NA_character_, times = ng)
        # select matchup without replacement
        for (tm in seq_len(ng / 2L)) {
            n1 <- 1L + 2L * (tm - 1L)
            n2 <- 2L * tm
            matchPairs[n1] <- ord[1L]
            ord <- ord[-1L]
            # don't pair same opponent twice
            isForbidden <- ord %in% unlist(results[matchPairs[n1], 
                oppRndIndex])
            ind <- ceiling(runif(n = 1L) * length(ord[!isForbidden]))
            if (ind < 1L) { ind <- 1L }
            opp <- ord[!isForbidden][ind]
            matchPairs[n2] <- opp
            ord <- ord[ord != opp]
        }
        # once new sequence is complete, done!
        if (!any(is.na(matchPairs))) { break }
        nit <- nit + 1L
        if (nit >= maxit) { 
            goAgain <- FALSE
            warning("could not create unique matchups")
        }
    }
    # fill in missing values
    unmatched <- !rownames(results) %in% na.omit(matchPairs)
    matchPairs[is.na(matchPairs)] <- rownames(results)[unmatched]
    return(matchPairs)
}


#' Pair All Teams
#' 
#' Returns a vector where each team in an odd position plays the
#' team in the following even position.
#' Teams on the same Total Games Won are preferentially paired.
#' If there is an odd number of teams on a given score, 
#' the team with the highest Matches Won in the next score down is paired.
#' 
# TODO try to avoid team getting pairdown more than once
#' @param results NULL or data frame with a row named for each team 
#' and with columns:\itemize{
#'     \item Total.Games.Won numeric total number of games won
#'     \item Matches.Won numeric total number of matches won
#'     \item Opponent.Round.X one or more columns naming the opponent in each round
#' }
#' @param maxit single integer number of times to repeat
#' @return character vector length number of rows results
#' @export
#' @examples
#' eg <- data.frame(Total.Games.Won = c(5, 4, 0, 1), 
#'     Matches.Won = c(1, 1, 0, 0), 
#'     Opponent.Round.1 = c("c", "d", "a", "b"), 
#'     stringsAsFactors = FALSE)
#' rownames(eg) <- letters[1:4]
#' makeAllMatchups(results = eg)

makeAllMatchups <- function(results) {
    # check results
    if (!is.data.frame(results)) {
        stop("results must be NULL or a data.frame")
    }
    nr <- nrow(results)
    if (nr %% 2 != 0) {
        warning("results should have an even number of rows") }
    expectCols <- c("Total.Games.Won", "Matches.Won")
    isMissingCols <- !expectCols %in% colnames(results)
    if (any(isMissingCols)) {
        stop("missing columns in results: ", 
            paste(expectCols[isMissingCols], collapse = ", "))
    }
    scrs <- sort(unique(results$Matches.Won), decreasing = TRUE)
    grps <- factor(results$Matches.Won, levels = scrs)
    # number of teams in each group
    numPerGrp <- tapply(X = grps, INDEX = grps, FUN = length)
    
    for (nn in seq_along(numPerGrp)) {
        # if number of teams in group is odd, 
        # pair down to team with best score in next lowest group
        if (numPerGrp[nn] %% 2L != 0L) {
            if (nn == length(numPerGrp)) {
                warning("odd number of teams in last group")
            } else {
                # choose pair up team
                isNextGrp <- grps == scrs[nn + 1]
                resultsNextGrp <- results[isNextGrp, ]
                resultsNextGrp <- resultsNextGrp[
                    resultsNextGrp$Total.Games.Won == 
                        max(resultsNextGrp$Total.Games.Won), 
                            "Total.Games.Won", drop = FALSE]
                pairUp <- rownames(resultsNextGrp)
                if (length(pairUp) > 1L) {
                    pairUp <- sample(x = pairUp, size = 1L)
                }
                # add pair up to Nth group
                ind <- rownames(results) == pairUp
                grps[ind] <- scrs[nn]
                numPerGrp[nn + 1L] <- numPerGrp[nn + 1L] - 1L
            }
        }
    }
    pairs <- character(nr)
    # pair within equal number of wins
    pos1 <- 1L
    pos2 <- 1L
    for (i in seq_along(scrs)) {
        
        index <- grps == scrs[i]
        if (sum(index) > 0L) {
            # do not change order of pairsScrs
            pairsScrs <- makeMatchup(
                results = results[index, , 
                    drop = FALSE])
            pos2 <- pos2 + sum(index) - 1L
            pairs[seq.int(from = pos1, to = pos2)] <- pairsScrs
            pos1 <- pos2 + 1L
            pos2 <- pos2 + 1L
        }
    }
    return(pairs)
}

