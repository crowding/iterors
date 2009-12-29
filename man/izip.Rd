\name{izip}
\alias{izip}
\title{Create an iterator over multiple iterables}
\description{
Create an iterator that iterates over multiple iterables, returning
the values as a list.
}
\usage{
izip(\dots)
}
\arguments{
  \item{\dots}{The iterables to iterate over.}
}
\examples{
# Iterate over two iterables of different sizes
as.list(izip(a=1:2, b=letters[1:3]))
}
\keyword{utilities}
