\name{isplitVector}
\alias{isplitVector}
\title{Create an iterator that splits a vector}
\description{
Create an iterator that splits a vector into smaller pieces.
You can specify either the number of pieces, using the \code{chunks}
argument, or the maximum size of the pieces, using the \code{chunkSize}
argument.
}
\usage{
isplitVector(x, \dots)
}
\arguments{
  \item{x}{Vector to iterate over.  Note that it doesn't need to be an
  atomic vector, so a list is acceptable.}
  \item{\dots}{Passed as the second and subsequent arguments to
  \code{idiv} function.  Currently, \code{idiv} accepts either a value
  for \code{chunks} or \code{chunkSize}.}
}
\value{
An iterator that returns vectors of the same type as \code{x} with one
or more elements from \code{x}.
}
\seealso{
  \code{\link[iterators]{idiv}}
}
\examples{
# Split the vector 1:10 into "chunks" with a maximum length of three
it <- ihasNext(isplitVector(1:10, chunkSize=3))
while (hasNext(it)) {
  print(nextElem(it))
}

# Split the vector "letters" into four chunks
it <- ihasNext(isplitVector(letters, chunks=4))
while (hasNext(it)) {
  print(nextElem(it))
}

# Get the first five elements of a list as a list
nextElem(isplitVector(as.list(letters), chunkSize=5))
}
\keyword{utilities}
