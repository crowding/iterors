\name{iRNGStream}
\alias{iRNGStream}
\alias{iRNGSubStream}
\title{Iterators that support parallel RNG}
\description{
The \code{iRNGStream} function creates an infinite iterator that calls
\code{nextRNGStream} repeatedly, and \code{iRNGSubStream} creates an
infinite iterator that calls \code{nextRNGSubStream} repeatedly.
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
it <- iRNGStream(313)
print(nextElem(it))
print(nextElem(it))

\dontrun{
library(foreach)
foreach(1:3, rseed=iRNGSubStream(1970), .combine='c') \%dopar\% {
  RNGkind("L'Ecuyer-CMRG") # would be better to initialize workers only once
  assign('.Random.seed', rseed, pos=.GlobalEnv)
  runif(1)
}
}
}
\keyword{utilities}
