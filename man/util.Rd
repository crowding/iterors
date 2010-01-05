\name{is.iterator}
\alias{is.iterator}
\alias{end_iteration}
\alias{iteration_has_ended}
\alias{new_iterator}
\title{Utilities for writing iterators}
\description{
  \code{is.iterator} indicates if an object is an iterator.
  \code{end_iteration} throws an exception to signal that there
  are no more values available in an iterator.
  \code{iteration_has_ended} tests an exception to see if it
  indicates that iteration has ended.
  \code{new_iterator} returns an iterator object.
}
\usage{
is.iterator(x)
end_iteration()
iteration_has_ended(e)
new_iterator(nextElem, \dots)
}
\arguments{
  \item{x}{any object.}
  \item{e}{a condition object.}
  \item{nextElem}{a function object that takes no arguments.}
  \item{\dots}{not currently used.}
}
\examples{
# Manually iterate using the iteration_has_ended function to help
it <- iter(1:3)
tryCatch({
  stopifnot(is.iterator(it))
  repeat {
    print(nextElem(it))
  }
},
error=function(e) {
  if (!iteration_has_ended(e)) {
    stop(e)
  }
})
}
\keyword{utilities}
