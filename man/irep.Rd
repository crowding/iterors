\name{irep}
\alias{irep}
\title{Create a repeating iterator}
\description{
Create an iterator version of the \code{rep} function.
}
\usage{
irep(iterable, times, length.out, each)
}
\arguments{
  \item{iterable}{The iterable to iterate over repeatedly.}
  \item{times}{The number of times to repeat the value.}
  \item{length.out}{The total number of values for the iterator to return.}
  \item{each}{The number of times to repeat each the value from the underlying iterator.}
}
\examples{
# Repeat 1, 2, and 3 until we've issued a total of 10 values
unlist(as.list(irep(1:3, length.out=10)))
}
\keyword{utilities}
