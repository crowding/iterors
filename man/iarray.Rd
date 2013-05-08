\name{iarray}
\alias{iarray}
\title{Create an iterator over an array}
\description{
Create an iterator over an array.
}
\usage{
iarray(X, MARGIN, ..., chunks, chunkSize, drop,
       idx=lapply(dim(X), function(i) TRUE))
}
\arguments{
  \item{X}{Array to iterate over.}
  \item{MARGIN}{Vector of subscripts to iterate over.  Note that if
     the length of \code{MARGIN} is greater than one, the resulting iterator
     will generate iterators which is particularly useful with nested
     foreach loops.}
  \item{\dots}{Used to force subsequent arguments to be specified by name.}
  \item{chunks}{Number of elements that the iterator should generate.
     This can be a single value or a vector the same length as \code{MARGIN}.
     A single value will be recycled for each dimension if \code{MARGIN}
     has more than one value.}
  \item{chunkSize}{The maximum size Number of elements that the iterator
     should generate.
     This can be a single value or a vector the same length as \code{MARGIN}.
     A single value will be recycled for each dimension if \code{MARGIN}
     has more than one value.}
  \item{drop}{Should dimensions of length 1 be dropped in the generated
     values?  It defaults to \code{FALSE} if either \code{chunks} or
     \code{chunkSize} is specified, otherwise to \code{TRUE}.}
  \item{idx}{List of indices used to generate a call object.}
}
\seealso{
  \code{\link[base]{apply}}
}
\examples{
  # Iterate over matrices in a 3D array
  x <- array(1:24, c(2,3,4))
  as.list(iarray(x, 3))

  # Iterate over subarrays
  as.list(iarray(x, 3, chunks=2))

  x <- array(1:64, c(4,4,4))
  it <- iarray(x, c(3,2), chunks=c(2,1))
  jt <- nextElem(it)
  nextElem(jt)
  jt <- nextElem(it)
  nextElem(jt)

  it <- iarray(x, c(3,2), chunks=c(2,2))
  jt <- nextElem(it)
  nextElem(jt)
  nextElem(jt)
  jt <- nextElem(it)
  nextElem(jt)
  nextElem(jt)
}
\keyword{utilities}
