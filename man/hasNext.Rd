\name{hasNext}
\alias{hasNext}
\alias{hasNext.ihasNext}
\title{Does This Iterator Have A Next Element}
\description{
  \code{hasNext} is a generic function that indicates if the iterator
  has another element.
}
\usage{
hasNext(obj, \dots)

\method{hasNext}{ihasNext}(obj, \dots)
}
\arguments{
  \item{obj}{an iterator object.}
  \item{\dots}{additional arguments that are ignored.}
}
\value{
  Logical value indicating whether the iterator has a next element.
}
\examples{
  it <- ihasNext(iter(c('a', 'b', 'c')))
  while (hasNext(it))
    print(nextElem(it))
}
\keyword{methods}
