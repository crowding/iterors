\name{product}
\alias{product}
\title{Create a cartesian product iterator}
\description{
Create an iterator that takes the cartesian product of all it's arguments.
}
\usage{
product(\dots)
}
\arguments{
  \item{\dots}{Named arguments to iterate over.}
}
\examples{
as.list(product(a=1:2, b=letters[1:3]))
}
\keyword{utilities}
