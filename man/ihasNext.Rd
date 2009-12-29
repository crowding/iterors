\name{ihasNext}
\alias{ihasNext}
\title{Create an iterator that supports the hasNext method}
\description{
  \code{ihasNext} is a generic function that indicates if the iterator
  has another element.
}
\usage{
ihasNext(iterable)
}
\arguments{
  \item{iterable}{an iterable object, which could be an iterator.}
}
\value{
  An \code{ihasNext} iterator that wraps the specified iterator and supports
  the \code{hasNext} method.
}
\examples{
  it <- ihasNext(c('a', 'b', 'c'))
  while (hasNext(it))
    print(nextElem(it))
}
\keyword{utilities}
