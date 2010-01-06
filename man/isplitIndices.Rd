\name{isplitIndices}
\alias{isplitIndices}
\title{Create an iterator of indices}
\description{
Create an iterator of chunks of indices from 1 to \code{n}.
You can specify either the number of pieces, using the \code{chunks}
argument, or the maximum size of the pieces, using the \code{chunkSize}
argument.
}
\usage{
isplitIndices(n, \dots)
}
\arguments{
  \item{n}{Maximum index to generate.}
  \item{\dots}{Passed as the second and subsequent arguments to
  \code{idiv} function.  Currently, \code{idiv} accepts either a value
  for \code{chunks} or \code{chunkSize}.}
}
\value{
An iterator that returns vectors of indices from 1 to \code{n}.
}
\seealso{
  \code{\link[iterators]{idiv}, \link{isplitVector}}
}
\examples{
# Return indices from 1 to 17 in vectors no longer than five
it <- ihasNext(isplitIndices(17, chunkSize=5))
while (hasNext(it)) {
  print(nextElem(it))
}

# Return indices from 1 to 7 in four vectors
it <- ihasNext(isplitIndices(7, chunks=4))
while (hasNext(it)) {
  print(nextElem(it))
}
}
\keyword{utilities}
