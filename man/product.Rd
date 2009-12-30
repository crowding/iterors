\name{product}
\alias{product}
\title{Create a cartesian product iterator}
\description{
Create an iterator that returns values from multiple iterators in
cartesian product fashion.  That is, they are combined the manner
of nested \code{for} loops.
}
\usage{
product(\dots)
}
\arguments{
  \item{\dots}{Named iterables to iterate over.  The right-most iterables
  change more quickly, like an odometer.}
}
\examples{
# Simulate a doubly-nested loop with a single while loop
it <- ihasNext(product(a=1:3, b=1:2))
while (hasNext(it)) {
  x <- nextElem(it)
  cat(sprintf('a = \%d, b = \%d\n', x$a, x$b))
}
}
\keyword{utilities}
