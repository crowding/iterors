\name{irep}
\alias{irep}
\title{Create a repeating iterator}
\description{
Create an iterator that returns a value a specified number of times.
}
\usage{
irep(x, times)
}
\arguments{
  \item{x}{The value to return repeatedly.}
  \item{times}{The number of times to repeat the value.  Default value is infinity.}
}
\examples{
# Repeat a value 10 times
unlist(as.list(irep(42, 10)))
}
\keyword{utilities}
