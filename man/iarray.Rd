\name{iarray}
\alias{iarray}
\title{Create an iterator over an array}
\description{
Create an iterator over an array. It is similar to the \code{iapply}
function from the iterators package, but it has special support for nested
foreach loops.
}
\usage{
iarray(X, MARGIN, ..., chunks, chunkSize, drop,
       idx=lapply(dim(X), function(i) TRUE), quote=FALSE)
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
  \item{idx}{List of length \code{length(dim(X))} containing indices
     used to create the expression that generates the iteration values.
     The default value contains all \code{TRUE} values,
     but you can use other values in order to iterate over a subset
     of \code{X}.
     See the example below for ways to do that.}
  \item{quote}{A logical value indicating whether the final iteration
     values should be quoted or not. This can be useful in a parallel
     context because it can be much more efficient to send call objects
     to parallel workers than the objects that they evaluate to.}
}
\seealso{
  \code{\link[base]{apply}}, \code{\link[iterators]{iapply}}
}
\examples{
  # Iterate over matrices in a 3D array
  x <- array(1:24, c(2,3,4))
  as.list(iarray(x, 3))

  # Iterate over subarrays
  as.list(iarray(x, 3, chunks=2))

  x <- array(1:64, c(4,4,4))
  it <- iarray(x, c(2,3), chunks=c(1,2))
  jt <- nextElem(it)
  nextElem(jt)
  jt <- nextElem(it)
  nextElem(jt)

  it <- iarray(x, c(2,3), chunks=c(2,2))
  jt <- nextElem(it)
  nextElem(jt)
  nextElem(jt)
  jt <- nextElem(it)
  nextElem(jt)
  nextElem(jt)

  # Use idx to iterate over only the middle four columns of a matrix
  x <- matrix(1:18, nrow=3, ncol=6)
  it <- iarray(x, 1, idx=list(TRUE, quote(2:5)))
  nextElem(it)
  nextElem(it)
  nextElem(it)

  # It can also be done using alist:
  it <- iarray(x, 1, idx=alist(, 2:5))
  nextElem(it)
  nextElem(it)
  nextElem(it)

  x <- matrix(1:1800, nrow=3, ncol=600)
  it <- iarray(x, 1, idx=alist(, 10:600), quote=TRUE)
  (yq <- nextElem(it))
  object.size(yq)
  object.size(eval(yq))
}
\keyword{utilities}
