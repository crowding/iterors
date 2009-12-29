\name{enumerate}
\alias{enumerate}
\title{Create an enumeration object}
\description{
Create an iterator that iterates over an iterable, returning the value
in a list that includes an index.
}
\usage{
enumerate(iterable)
}
\arguments{
  \item{iterable}{Iterable to iterate over.}
}
\examples{
# Create an enumeration of five random numbers
as.list(enumerate(rnorm(5)))
}
\keyword{utilities}
