\name{isplitRows}
\alias{isplitRows}
\title{Create an iterator that splits a matrix into block rows}
\description{
Create an iterator that splits a matrix into block rows.
You can specify either the number of blocks, using the \code{chunks}
argument, or the maximum size of the blocks, using the \code{chunkSize}
argument.
}
\usage{
isplitRows(x, \dots)
}
\arguments{
  \item{x}{Matrix to iterate over.}
  \item{\dots}{Passed as the second and subsequent arguments to
  \code{idiv} function.  Currently, \code{idiv} accepts either a value
  for \code{chunks} or \code{chunkSize}.}
}
\value{
An iterator that returns submatrices of \code{x}.
}
\seealso{
  \code{\link[iterators]{idiv}, \link{isplitCols}}
}
\examples{
# Split a matrix into submatrices with a maximum of three rows
x <- matrix(1:100, 10)
it <- ihasNext(isplitRows(x, chunkSize=3))
while (hasNext(it)) {
  print(nextElem(it))
}

# Split the same matrix into five submatrices
it <- ihasNext(isplitRows(x, chunks=5))
while (hasNext(it)) {
  print(nextElem(it))
}
}
\keyword{utilities}
