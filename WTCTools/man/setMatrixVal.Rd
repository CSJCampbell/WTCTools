% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMatrixVal.R
\name{setMatrixVal}
\alias{setMatrixVal}
\title{Set a value in a Matrix}
\usage{
setMatrixVal(x, list1, list2, val, type = c("neg", "same", "0-1"))
}
\arguments{
\item{x}{named matrix}

\item{list1}{single character name of player 1 list}

\item{list2}{single character name of player 2 list}

\item{val}{single numeric value to set}

\item{type}{single character specify how opposite pair should be entered: \itemize{
    \item neg negative
    \item same equal to pair
    \item 0-1 as 1 - pair (since pair is fraction)
}
negative or save sign? (default TRUE)}
}
\value{
matrix
}
\description{
Updater function for named matrix
}
\examples{
m1 <- matrix(0, nrow = 3, ncol = 3, 
    dimnames = list(letters[1:3], letters[1:3])) / 10
setMatrixVal(list1 = "b", list2 = "a", x = m1, val = -2)
setMatrixVal(list1 = c("a", "b", "c"), 
    list2 = c("b", "c", "a"), x = m1, val = 1:3)
}
