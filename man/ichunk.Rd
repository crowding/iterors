\name{ichunk}
\alias{ichunk}
\title{Create a chunking iterator}
\description{
Create an iterator that issues lists of values from the underlying iterable.
This is useful for manually \dQuote{chunking} values from an iterable.
}
\usage{
ichunk(iterable, chunkSize)
}
\arguments{
  \item{iterable}{Iterable to iterate over.}
  \item{chunkSize}{Maximum number of values from \code{iterable} to
  return in each value issued by the resulting iterator.}
}
\seealso{
  \code{\link{isplitVector}}
}
\examples{
# Split the vector 1:10 into "chunks" with a maximum length of three
it <- ihasNext(ichunk(1:10, 3))
while (hasNext(it)) {
  print(unlist(nextElem(it)))
}
}
\keyword{utilities}
