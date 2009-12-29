\name{chain}
\alias{chain}
\title{Create a chaining iterator}
\description{
Create an iterator that chains multiple iterables together.
}
\usage{
chain(\dots)
}
\arguments{
  \item{\dots}{The iterables to iterate over.}
}
\examples{
# Iterate over two iterables
as.list(chain(1:2, letters[1:3]))
}
\keyword{utilities}
