% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ichunk.R
\name{i_chunk}
\alias{i_chunk}
\title{Combine an iterator's values into chunks.}
\usage{
i_chunk(iterable, size, mode = "list", fill, ...)
}
\arguments{
\item{iterable}{Iterable to iterate over.}

\item{size}{Maximum number of values from \code{iterable} to return
in each value issued by the resulting iterator.}

\item{mode}{Mode of the objects returned by the iterator.}

\item{fill}{Value to use to pad the last chunk to size, if it is
short. If missing, no padding will be done.}

\item{...}{Further arguments will be forwarded to \code{iteror(iterable, ...)}.}
}
\value{
an iteror that yields items of length \code{size} and mode \code{mode}.
}
\description{
Create an iterator that issues lists of values from the underlying iterable.
This is useful for manually \dQuote{chunking} values from an iterable.
}
\examples{

# Split the vector 1:10 into "chunks" with a maximum length of three
it <- i_chunk(1:10, 3)
repeat print(unlist(nextOr(it, break)))

# Same as previous, but return integer vectors rather than lists
it <- i_chunk(1:10, 3, mode='integer')
repeat print(unlist(nextOr(it, break)))

it <- i_chunk(iterators::iter(1:5), 2, fill=NA)
# List: list(1, 2, 3)
nextOr(it, NULL)
# List: list(4, 5, NA)
nextOr(it, NULL)

it2 <- i_chunk(levels(iris$Species), 4, fill="weeee")
# Returns: list("setosa", "versicolor", "virginica", "weeee")
nextOr(it2, NA)

}
\seealso{
\code{iteror.default}

Argument \code{size} does not need to be an integer, for instance a
\code{chunk} of 3.5 will produce chunks of sizes 3 and 4
alternating. The precise behavior will be subject to floating
point precision.
}
