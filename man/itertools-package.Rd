\name{itertools-package}
\alias{itertools-package}
\alias{itertools}
\docType{package}
\title{
The itertools Package
}
\description{
The \code{itertools} package provides a variety of functions used to create
iterators, as defined by REvolution Computing's \code{iterators} package.
Many of the functions are patterned after functions of the same name in
the Python itertools module, including \code{chain}, \code{product},
\code{izip}, \code{ifilter}, etc.
In addition, a number of functions were inspired by utility functions in the
\code{snow} package, such as \code{isplitRows}, \code{isplitCols}, and
\code{isplitIndices}.

There are also several utility functions that were contributed by
Hadley Wickham that aid in writing iterators.
These include \code{is.iterator}, \code{end_iterator},
\code{iteration_has_ended}, and \code{new_iterator}.
}
\details{
More information is available on the following topics:
\tabular{ll}{
\code{isplitVector} \tab splits, or slices, a vector into shorter segments\cr
\code{isplitCols} \tab splits a matrix column-wise\cr
\code{isplitRows} \tab splits a matrix row-wise\cr
\code{isplitIndices} \tab iterate over \dQuote{chunks} of indices from 1 to n\cr
\code{chain} \tab chain multiple iterators together into one iterator\cr
\code{enumerate} \tab create an enumeration from an iterator\cr
\code{ichunk} \tab create lists of values from an iterator to aid manual chunking\cr
\code{ihasNext} \tab add a hasNext method to an iterator\cr
\code{ifilter} \tab only return values for which a predicate function returns true\cr
\code{ifilterfalse} \tab only return values for which a predicate function returns false\cr
\code{ilimit} \tab limit, or truncate, an iterator\cr
\code{irep} \tab create a repeating iterator\cr
\code{izip} \tab zip together multiple iterators\cr
\code{product} \tab zip together multiple iterators in cartesian product fashion\cr
\code{recycle} \tab recycle values from an iterator repeatedly\cr
\code{is.iterator} \tab indicates if an object is an iterator\cr
\code{end_iteration} \tab throws an exception to signal end of iteration\cr
\code{iteration_has_ended} \tab tests an exception to see if iteration has ended\cr
\code{new_iterator} \tab creates a new iterator object\cr
}

For a complete list of functions with individual help pages,
use \code{library(help="itertools")}.
}
\keyword{package}
