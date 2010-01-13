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
  \item{times}{A vector giving the number of times to repeat each element
  if the length is greater than one, or to repeat all the elements if the
  length is one.  This behavior is less strict than \code{rep} since the
  length of an iterable isn't generally known.}
  \item{length.out}{non-negative integer.  The desired length of the output
  iterator.}
  \item{each}{non-negative integer.  Each element of the iterable is repeated
  \code{each} times.}
}
\seealso{
  \code{\link[base]{rep}}
}
\examples{
unlist(as.list(irep(1:4, 2)))
unlist(as.list(irep(1:4, each=2)))
unlist(as.list(irep(1:4, c(2,2,2,2))))
unlist(as.list(irep(1:4, c(2,1,2,1))))
unlist(as.list(irep(1:4, each=2, len=4)))
unlist(as.list(irep(1:4, each=2, len=10)))
unlist(as.list(irep(1:4, each=2, times=3)))
}
\keyword{utilities}
