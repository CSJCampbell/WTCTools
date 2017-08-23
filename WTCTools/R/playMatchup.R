
#' Simulate the result of a Match
#' 
#' Get the results of 5 games between 10 players in each of two teams.
#' 
#' @param team1 single character name of Team in data
#' @param team2 single character name of Team in data
#' @param data data frame with columns:\itemize{
#'     \item Team
#'     \item Player
#'     \item List
#' }
#' @param ratings rating object
#' @param pairlookup matrix with gamma correction for 
#' matchup pairing with column and row names in List
#' @param round single integer
#' @return length 5 numeric
#' @export
#' @examples
#' set.seed(43626)
#' playMatchup(team1 = "China", team2 = "Canada Goose", 
#'     data = dwtc2015, ratings = rating2015, pairlookup = pairLookup)
#' ## [1] 0 1 1 0 1

playMatchup <- function(team1, team2, 
    data, ratings, pairlookup, round = 7) {
    # check inputs
    if (missing(team1)) { stop("team1 is missing") }
    if (missing(team2)) { stop("team2 is missing") }
    if (missing(data)) { stop("data is missing") }
    if (missing(ratings)) { stop("ratings is missing") }
    if (is(object = ratings, class2 = "ratings")) { 
        stop("ratings must be a ratings object") }
    if (missing(pairlookup)) { stop("pairlookup is missing") }
    if (!is.matrix(pairlookup)) { stop("pairlookup must be a matrix") }
    if (!all(rownames(pairlookup) == colnames(pairlookup))) {
        stop("pairlookup must have symmetric row names and column names") }
    if (!is.data.frame(data)) { stop("data must be a data.frame") }
    if (!all(c("Team", "Player", "List") %in% colnames(data))) { 
        stop("data must have columns 'Team', 'Player' and 'List'") }
    if (!all(c(team1, team2) %in% data$Team)) { 
        stop(paste0("team1 (", team1, ") and team2 (", team2, 
            ") must be present in data$Team")) }
    
    team1Players <- unique(data$Player[data$Team == team1])
    team2Players <- unique(data$Player[data$Team == team2])
    
    found <- c(team1Players, team2Players) %in% ratings$ratings$Player
    if (!all(found)) {
        stop("players missing in ratings:\n", 
            paste(c(team1Players, team2Players)[!found], collapse = ", ")) }
    
    team1Lists <- data$List[data$Team == team1]
    team2Lists <- data$List[data$Team == team2]
    
    if (!all(c(team1Lists, team2Lists) %in% rownames(pairlookup))) {
        stop("all Lists must be present in pairlookup") }
    
    dat <- makePairing(team1players = team1Players, 
        team2players = team2Players, 
        team1lists = team1Lists, 
        team2lists = team2Lists, round = round)
    
    prob <- predict(ratings, 
        dat[, c("round", "team1players", "team2players")], 
        tng = 0, gamma = getMatrixVal(
            x = pairlookup,
            list1 = dat[, "team1lists"], 
            list2 = dat[, "team2lists"]))
    resGame <- rbinom(n = 5, size = 1, prob = prob)
    return(resGame)
}


#' Make Random Pairings
#' 
#' Each player meets randomly.
#' Each player selects their list randomly.
#' 
#' @param team1players length 5 or 10 character vector 
#' @param team2players length 5 or 10 character vector 
#' @param team1lists length 10 character vector 
#' @param team2lists length 10 character vector 
#' @param round single integer match round
#' @return data frame with 5 rows and columns:\enumerate{
#'     \item round
#'     \item team1players
#'     \item team2players
#'     \item team1lists
#'     \item team2listss
#' }
#' @export
#' @examples
#' makePairing(team1players = letters[1:5], 
#'     team2players = letters[11:15], 
#'     team1lists = LETTERS[1:10], 
#'     team2lists = LETTERS[11:20])

makePairing <- function(team1players, team2players, 
    team1lists, team2lists, round = 7) {
    
    if (!length(team1players) == 5 || length(team1players) == 10) {
        stop("team1players must be length 5 (or 10)")
    }
    if (!length(team2players) == 5 || length(team2players) == 10) {
        stop("team2players must be length 5 (or 10)")
    }
    if (length(team1lists) != 10) {
        stop("team1lists must be length 10")
    }
    if (length(team2lists) != 10) {
        stop("team2lists must be length 10")
    }
    # random pairings
    rand <- runif(n = 10)
    # shuffle players
    team1players <- unique(team1players)[order(rand[1:5])]
    team2players <- unique(team2players)[order(rand[6:10])]
    # choose list (needs ordering to match player order)
    
    dat <- data.frame(round = round, 
        team1players = team1players, 
        team2players = team2players, 
        team1lists = team1lists[splitPairs(n = 10L)][order(rand[1:5])],
        team2lists = team2lists[splitPairs(n = 10L)][order(rand[6:10])], 
        stringsAsFactors = FALSE)
    return(dat)
}


#' Split a vector
#' 
#' Splits sequential pairs in a vector randomly into two groups.
#'  
#' @param n single even integer
#' @return logical vector length n
#' @export
#' @examples
#' splitPairs(n = 2)
#' splitPairs(n = 4)

splitPairs <- function(n = 10L) {
    if (length(n) != 1L) { 
        stop("n must be length one")
    }
    if (n %% 2 != 0) { 
        warning("n should be even")
    }
    rand <- runif(n / 2)
    cl <- numeric(n)
    cl[seq_len(n / 2) * 2] <- rand
    cl[(seq_len(n / 2) * 2) - 1] <- 1 - rand
    cl[cl >= 0.5] <- 1
    cl[cl < 0.5] <- 0
    return(as.logical(cl))
}

