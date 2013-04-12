\name{iRNGStream}
\alias{iRNGStream}
\alias{iRNGSubStream}
\title{Iterators that support parallel RNG}
\description{
The \code{iRNGStream} function creates an iterator that calls \code{nextRNGStream}
repeatedly, and \code{iRNGSubStream} creates an iterator that calls
\code{nextRNGSubStream} repeatedly.
}
\usage{
iRNGStream(seed)
iRNGSubStream(seed)
}
\arguments{
  \item{seed}{Either a single number to be passed to \code{set.seed} or a vector
              to be passed to \code{nextRNGStream} or \code{nextRNGSubStream}.}
}
\seealso{
  \code{\link[base]{set.seed}}, \code{\link[parallel]{nextRNGStream}},
  \code{\link[parallel]{nextRNGSubStream}}
}
\examples{
it <- iRNGStream(42)
print(nextElem(it))
print(nextElem(it))
}
\keyword{utilities}
