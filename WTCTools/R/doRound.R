

#' Get Results for a Round
#'
#' Given Players in Teams with Lists, with ratings and taking into 
#' account list pairs, calculate results for a given round.
#' Take into account results for previous rounds if provided.
#' @param data data frame with columns:\itemize{
#'     \item Team
#'     \item Player
#'     \item List
#' }
#' @param round single integer
#' @param results NULL or data frame with a row named for each team 
#' and with columns:\itemize{
#'     \item Total.Games.Won numeric total number of games won
#'     \item Matches.Won numeric total number of matches won
#'     \item Opponent.Round.X one or more columns naming the opponent in each round
#' }
#' @param ratings \link[PlayerRatings]{rating} object
#' @param pairlookup matrix of List pairings
#' @param pairs character vector of unique teams
#' @return data frame with rownames Team, and columns Total.Games.Won, Matches.Won, 
#' and Opponent.Round.X, where X is the value of round
#' @export
#' @examples
#' rat <- PlayerRatings::steph(x = wtc2014[, c("round", "player1", "player2", "TP")])
#' players2014 <- unique(c(wtc2014$player1, wtc2014$player2))
#' players2015 <- unique(wtc2015_players$Player)
#' rat$ratings <- rbind(rat$ratings, 
#'     data.frame(Player = players2015[!players2015 %in% players2014], 
#'         Rating = 2200, Deviation = 300, 
#'         Games = 0, Win = 0, Draw = 0, Loss = 0, Lag = 0))
#' doRound(data = wtc2015_players, results = NULL, 
#'     ratings = rat, pairlookup = initializeLookup(data = wtc2015_players$List))

doRound <- function(data, round = 7, results = NULL, 
    ratings, pairlookup, pairs = NULL) {
    if (!all(c("Team", "Player", "List") %in% colnames(data))) { 
        stop("data must have columns 'Team', 'Player' and 'List'") }
    if (missing(ratings)) { stop("ratings is missing") }
    if (is(object = ratings, class2 = "ratings")) { 
        stop("ratings must be a ratings object") }
    if (missing(pairlookup)) { stop("pairlookup is missing") }
    if (!is.matrix(pairlookup)) { stop("pairlookup must be a matrix") }
    if (!all(rownames(pairlookup) == colnames(pairlookup))) {
        stop("pairlookup must have symmetric row names and column names")
    }
    
    teams <- unique(data$Team)
    nr <- length(teams)
    
    if (!is.null(pairs)) {
        if (!all(sort(pairs) == sort(teams))) {
            stop("pairs should be the unique pairings for teams")
        }
    }
    isFirstRound <- is.null(results)
    
    if (is.null(pairs)) {
        if (isFirstRound) {
            # dodge same country pairings
            isGrp1 <- splitPairs(n = nr)
            pairs <- c(teams[isGrp1][order(runif(n = nr / 2))], 
                teams[!isGrp1][order(runif(n = nr / 2))])
            
        } else {
            # TODO try to dodge same country pairings
            pairs <- makeAllMatchups(results = results)
        }
    }
    # get results for pairs
    matchRound <- numeric(nr)
    names(matchRound) <- pairs
    gamesRound <- numeric(nr)
    names(gamesRound) <- pairs
    opponentRound <- character(nr)
    names(opponentRound) <- pairs
    for (matchup in seq_len(nr / 2)) {
        n1 <- 1 + 2 * (matchup - 1)
        n2 <- 2 * matchup
        res <- playMatchup(team1 = pairs[n1], team2 = pairs[n2], 
            data = data, ratings = ratings, pairlookup = pairlookup)
        
        isTeam1Win <- sum(res) >= 3
        matchRound[c(n1, n2)] <- c(isTeam1Win, !isTeam1Win)
        gamesRound[c(n1, n2)] <- c(sum(res), 5 - sum(res))
        opponentRound[c(n1, n2)] <- c(pairs[n2], pairs[n1])
    }
    orn <- paste0("Opponent.Round.", round)
    
    # initialize output
    if (isFirstRound) {
        results <- data.frame(Total.Games.Won = rep(0, times = nr), 
            Matches.Won = 0, 
            Opponent.Round = 0)
        colnames(results)[3] <- orn
        rownames(results) <- teams
    }
    # update results
    results[teams, "Total.Games.Won"] <- results[teams, "Total.Games.Won"] + 
        gamesRound[teams]
    results[teams, "Matches.Won"] <- results[teams, "Matches.Won"] + 
        matchRound[teams]
    results[teams, orn] <- opponentRound[teams]
    
    return(results)
}
