\name{ibreak}
\alias{ibreak}
\title{Create an iterator that can be told to stop}
\description{
Create an iterator that iterates over another iterator until
a specified function returns \code{FALSE}.
This can be useful for breaking out of a foreach loop, for example.
}
\usage{
ibreak(iterable, finished)
}
\arguments{
  \item{iterable}{Iterable to iterate over.}
  \item{finished}{Function that returns a logical value.  The iterator
    stops when this function returns \code{FALSE}.}
}
\examples{
# See how high we can count in a tenth of a second
mkfinished <- function(time) {
  starttime <- proc.time()[3]
  function() proc.time()[3] > starttime + time
}
length(as.list(ibreak(icount(), mkfinished(0.1))))
}
\keyword{utilities}
