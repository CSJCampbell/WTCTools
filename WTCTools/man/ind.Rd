% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ind}
\alias{ind}
\title{Create a pair reversing index}
\usage{
ind(n)
}
\arguments{
\item{n}{single even integer}
}
\value{
integer vector of length n
}
\description{
Provide an even integer. Create an index vector to reverse paired values in 
a vector.
}
\examples{
ind(n = 4)
## [1] 2 1 4 3
ind(n = 6)
## [1] 2 1 4 3 6 5
}
