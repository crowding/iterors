\name{ilimit}
\alias{ilimit}
\title{Create a limited iterator}
\description{
Create an iterator that wraps a specified iterable a limited number of times.
}
\usage{
ilimit(iterable, n)
}
\arguments{
  \item{iterable}{Iterable to iterate over.}
  \item{n}{Maximum number of values to return.}
}
\examples{
# Limit icount to only return three values
as.list(ilimit(icount(), 3))
}
\keyword{utilities}
