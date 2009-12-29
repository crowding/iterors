\name{ifilter}
\alias{ifilter}
\alias{ifilterfalse}
\title{Create a filtering iterator}
\description{
The \code{ifilter} and \code{ifilterfalse} functions create iterators
that return a subset of the values of the specified iterable.
\code{ifilter} returns the values for which the \code{pred} function
returns \code{TRUE}, and \code{ifilterfalse} returns the values for
which the \code{pred} function returns \code{FALSE}.
}
\usage{
ifilter(pred, iterable)
ifilterfalse(pred, iterable)
}
\arguments{
  \item{pred}{A function that takes one argument and returns \code{TRUE} or \code{FALSE}.}
  \item{iterable}{The iterable to iterate over.}
}
\examples{
# Return the odd numbers between 1 and 10
as.list(ifilter(function(x) x \%\% 2 == 1, icount(10)))

# Return the even numbers between 1 and 10
as.list(ifilterfalse(function(x) x \%\% 2 == 1, icount(10)))
}
\keyword{utilities}
