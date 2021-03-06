% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStat.R
\name{getNewStat}
\alias{getNewStat}
\title{Set Matrix Value, then Get Statistic}
\usage{
getNewStat(val, data, status = NULL, pairlookup, pair,
  compare = "scorefrac", round = "round", player1 = "player1",
  player2 = "player2", result = "result", list1 = "list1",
  list2 = "list2")
}
\arguments{
\item{val}{numeric value for pairlookup at pair}

\item{data}{data.frame with columns round, 
player1, player2, result, list1, list2, scorefrac}

\item{status}{data frame containing prior ratings (from class rating)}

\item{pairlookup}{matrix with names in list1}

\item{pair}{length two vector with names in list1}

\item{compare}{single character name of comparison column (default "scorefrac")}

\item{round}{single character name of round column (default "round")}

\item{player1}{single character name of player one column (default "player1")}

\item{player2}{single character name of player two column (default "player2")}

\item{result}{single character name of result column (default "result")}

\item{list1}{single character name of player one's list column (default "list1")}

\item{list2}{single character name of player two's list column (default "list2")}
}
\value{
single numeric optimized difference between scorefrac 
and p-steph for a player, given a list pair lookup
}
\description{
Minimization tool for setting pairlookup, 
then getting statistic from difference between 
predicted value and scorefrac using \code{\{link{getStat}}.
}
\examples{
\dontrun{
getNewStat(val = 10, data = wtct, 
    pairlookup = look, pair = c("Absylonia2", "Baldur2"))
# [1] 87.14286
}
pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
    dimnames = list(c("a", "b"), c("a", "b")))
dat <- data.frame(round = rep(1:6, each = 2),
    player1 = rep(c("A", "B"), times = 2), 
    player2 = rep(c("B", "A"), times = 2),
    result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
    list1 = c("a", "b", "b", "a"), 
    list2 = c("b", "a", "a", "b"), 
    scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
    stringsAsFactors = FALSE)
getNewStat(val = 60, data = dat, pairlookup = pl, 
    pair = c("a", "b"))
}
