
#' Create Score for Predicted Sequence
#'
#' The score is the mean distance a guess position is from 
#' the actual result. The score is sum of the absolute distance between
#' the predictions and the actual results, divided by the number 
#' of guesses.
#'
#' To allow people to guess country rather than team, there 
#' is also handling for partial matches of teams so that "Team USA" matches
#' "Team USA Stars" and "Team USA Stripes". This will be scored as one 
#' guess which has the average distance to each match.
#' 
#' @param guess character vector, 1 or more elements of result
#' starting from position 1
#' @param result character vector, comparison values
#' @return single numeric
#' @export
#' @examples
#' scoreSequence(guess = c("c", "b", "a"), result = letters) # 4/3 (3)
#' scoreSequence(guess = letters[1:3], result = letters) # 0 (3)
#' scoreSequence(guess = c("a", "a", "a"), result = letters) # 1 (3)
#' # aa not found (26), ab is 1 away, a is 2 away
#' scoreSequence(guess = c("aa", "ab", "a"), result = paste0(letters, "b")) # 29/3 (3)
#' scoreSequence(guess = "Australia", result = leaderboard15$Team) # 2 (1)
#' # distance is 0 + 0 + 0 + 3 + 4 / 5
#' scoreSequence(guess = c("a", "b", "c"), result = c("a A", "b B", "c C", "a B", "a C")) # 7/3


scoreSequence <- function(guess, result) {
    
    if (missing(guess)) { stop("guess is missing") }
    if (missing(result)) { stop("result is missing") }
    if (length(guess) < 1) { stop("guess is zero length") }
    if (length(result) < 1) { stop("result is zero length") }
    
    if (any(duplicated(result))) {
        stop("duplicated entries in result")
    }
    guess[guess == ""] <- NA
    ng <- length(na.omit(guess))
    nr <- length(result)
    if (ng > 0L) {
        exact <- FALSE
        if (length(guess) == length(result) && all(guess %in% result)) { 
            exact <- TRUE
        }
        res <- integer(length(guess))
        if (exact) {
            res <- abs(seq_len(ng) - order(as.integer(factor(guess, labels = result))))
        } else {
            for (i in seq_along(guess)) {
                if (!is.na(guess[i])) {
                    # partial matching
                    found <- which(grepl(pattern = guess[i], x = result))
                    # expect that there might be no matches
                    if (length(found) < 1) {
                        warning(guess[i], " not found")
                        res[i] <- nr
                    } else {
                        for (match in seq_along(found)) {
                            if (match == 1L) {
                                # guess is position found[match]
                                # position is i
                                # difference is:
                                res[i] <- abs(i - found[match])
                            } else {
                                # add any additional hits found twice, 
                                # since guessing either position
                                res <- c(res, abs(i - found[match]))
                                ng <- ng + 1L
                            }
                        }
                    }
                }
            }
        }
        # average distance
        out <- sum(res) / ng
    } else {
        out <- NA_integer_
    }
    # number of guesses
    attr(x = out, which = "n") <- length(na.omit(guess))
    return(out)
}


#' @name leaderboard15
#' @title World Team Championships 2015 Team Leaderboard
#' @description 
#' Team results for WTC 2015.
#' 
#' @docType data
#' @format Data frame with columns:\itemize{
#'     \item  Rank integer noting team position
#'     \item  Team character name of team
#'     \item  Rounds.won integer number of rounds won
#'     \item  Team.SoS integer strength of schedule
#'     \item  Games.won integer number of games won
#'     \item  Total.CP integer control points scored
#'     \item  Total.VP integer army points scored
#' }
#' @keywords datasets
#' @examples
#' hist(leaderboard15$Total.CP, col = "#99334455")

NULL
