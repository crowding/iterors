\name{timeout}
\alias{timeout}
\title{Create a timeout iterator}
\description{
Create an iterator that iterates over another iterator for a specified
period of time, and then stops.  This can be useful when you want to
search for something, or run a test for awhile, and then stop.
}
\usage{
timeout(iterable, time)
}
\arguments{
  \item{iterable}{Iterable to iterate over.}
  \item{time}{The time interval to iterate for, in seconds.}
}
\examples{
# See how high we can count in a tenth of a second
length(as.list(timeout(icount(), 0.1)))
}
\keyword{utilities}
