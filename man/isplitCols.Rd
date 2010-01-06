\name{isplitCols}
\alias{isplitCols}
\title{Create an iterator that splits a matrix into block columns}
\description{
Create an iterator that splits a matrix into block columns.
You can specify either the number of blocks, using the \code{chunks}
argument, or the maximum size of the blocks, using the \code{chunkSize}
argument.
}
\usage{
isplitCols(x, \dots)
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
  \code{\link[iterators]{idiv}, \link{isplitRows}}
}
\examples{
# Split a matrix into submatrices with a maximum of three columns
x <- matrix(1:30, 3)
it <- ihasNext(isplitCols(x, chunkSize=3))
while (hasNext(it)) {
  print(nextElem(it))
}

# Split the same matrix into five submatrices
it <- ihasNext(isplitCols(x, chunks=5))
while (hasNext(it)) {
  print(nextElem(it))
}
}
\keyword{utilities}
