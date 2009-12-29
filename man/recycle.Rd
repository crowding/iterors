\name{recycle}
\alias{recycle}
\title{Create a recycling iterator}
\description{
Create an iterator that recycles a specified iterable.
}
\usage{
recycle(iterable)
}
\arguments{
  \item{iterable}{The iterable to recycle.}
}
\examples{
# Recycle over 'a', 'b', and 'c' three times
unlist(as.list(recycle(letters[1:3]), 9))
}
\keyword{utilities}
