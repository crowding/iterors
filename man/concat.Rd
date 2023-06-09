% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concat.R
\name{concat}
\alias{concat}
\alias{concat.default}
\alias{concat.iteror}
\title{Concatenate contents of multiple iterators into a vector.}
\usage{
concat(obj, mode = "list", n = Inf, ...)

\method{concat}{default}(obj, mode = "list", n = as.integer(2^31 - 1), ...)

\method{concat}{iteror}(obj, mode = "list", n = Inf, length.out = Inf, ...)
}
\arguments{
\item{obj}{An iteror.}

\item{mode}{The mode of vector to return.}

\item{n}{The maximum number of times to call \code{nextOr(obj)}.}

\item{...}{passed along to \link{iteror} constructor.}

\item{length.out}{The target size of the output vector (after
results have been pasted together). If the iteror ends (or emits
\code{n} results) before emitting this many elements, the result will be
shorter than \code{length.out}. If the iterator does not end early, the output
will have at least \code{length.out} elements, and possibly more,
as the entire last chunk will be included.}
}
\value{
a vector with mode \code{mode}.
}
\description{
\code{concat} collects all values from an iterable object, and pastes
them end to end into one vector.  In other words \code{concat} is to
\code{as.list.iteror} as \code{c} is to \code{list}.
}
\examples{

it <- i_apply(icount(), seq_len) # [1], [1, 2], [1, 2, 3], ...
concat(it, n=4, mode="numeric")  # [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]
concat(it, length.out=4, mode="numeric")  # [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]
}
