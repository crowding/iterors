\name{recycle}
\alias{recycle}
\title{Create a recycling iterator}
\description{
Create an iterator that recycles a specified iterable.
}
\usage{
recycle(iterable, times=NA_integer_)
}
\arguments{
  \item{iterable}{The iterable to recycle.}
  \item{times}{integer.  Number of times to recycle the values in the iterator.
  Default value of \code{NA_integer_} means to recycle forever.}
}
\examples{
# Recycle over 'a', 'b', and 'c' three times
recycle(letters[1:3], 3)
}
\keyword{utilities}
