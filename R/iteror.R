# Copyright (c) 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA


#' Iterator Factory Functions
#'
#' \code{iter} is a generic function used to create iterator objects.
#'
#'
#' @aliases iter iter.default iter.iter iter.matrix iter.data.frame
#' iter.function
#' @param obj an object from which to generate an iterator.
#' @param \dots additional arguments affecting the iterator.
#' @return The iterator.
#' @keywords methods
#' @examples
#'
#' # a vector iterator
#' i1 <- iteror(1:3)
#' nextOr(i1)
#' nextOr(i1)
#' nextOr(i1)
#'
#' # a vector iterator with a checkFunc
#' i1 <- iteror(1:3, checkFunc = function(i) i%%2 == 0)
#' nextOr(i1)
#'
#' # a data frame iterator by column
#' i2 <- iteror(data.frame(x = 1:3, y = 10, z = c("a", "b", "c")))
#' nextOr(i2)
#' nextOr(i2)
#' nextOr(i2)
#'
#' # a data frame iterator by row
#' i3 <- iteror(data.frame(x = 1:3, y = 10), by = "row")
#' nextOr(i3)
#' nextOr(i3)
#' nextOr(i3)
#'
#' # a function iterator
#' i4 <- iteror(function() rnorm(1), sigil=NULL)
#' nextOr(i4)
#' nextOr(i4)
#' nextOr(i4)
#'

#' An efficient and compact iteration protocol.
#'
#' To create an iteror, call the constructor `iteror` providing either
#' a vector or a function as argument. The returned object will
#' support the method [nextOr(obj, or)] to extract successive
#' values.
#'
#' The main method for "iteror" is "nextOr" rather than
#' "nextElem". Instead of using exceptions, "nextOr" uses a lazily
#' evaluated "or" argument to signal the end of iteration.  The "or"
#' argument will only be forced when end of iteration is reached; this
#' means the consumer can provide an action like "break", "next" or
#' "return" to take at the the end of iteration. Summing over an
#' iteror this way looks like:
#'
#' ```
#' sum <- 0
#' it <- iteror(in)
#' repeat {
#'   val <- nextOr(iter, break)
#'   sum <- sum + val;
#' }
#' ```
#'
#' Another way to use the "or" argument is to give it a sigil value;
#' that is, a special value that will be interpreted as end of
#' iteration.  If the result of calling `nextOr` is `identical()` to
#' the sigil value you provided, then you know the iterator has
#' ended. In R it is commonplace to use `NULL` or `NA`, in the role of
#' a sigil, but that only works until you have an iterator that needs
#' to yield NULL. A safer alternative is to use a one-shot sigil
#' value; `new.env()` is a good choice, as it produces an object that
#' by construction is not [identical] to any other object in the R
#' session. This pattern looks like:
#'
#' ```
#' sum <- 0
#' stopped <- new.env()
#' repeat {
#'   val <- nextOr(iter, stopped)
#'   if (identical(val, stopped)) break
#'   sum <- sum + val
#' }
#' ```
#'
#' @export
#' @param obj An object to iterate with. If `obj` is a vector, the
#'   iterator will go over the elements of that vector and you can use
#'   `recycle`.  If `obj` is a function, the function will be called
#'   to compute successive elements. The function should have a leading
#'   argument `or` and behave accordingly (only forcing and returning
#'   `or` to signal end of iteration.)  If you provide a function that
#'   does not have an `or` argument, you will need to specify either `catch`
#'   or `sigil`.
#' @return an object of classes 'iteror' and 'iter'.
iteror <- function(obj, ...) {
  UseMethod("iteror")
}

#' @exportS3Method
iteror.iteror <- identity

#' @exportS3Method
iteror.iter <- identity

#' @exportS3Method iteror "function"
#' @rdname iteror
#' @param catch If `obj` is a function without an `or` argument, specify
#'   e.g. `catch="StopIteration"` to interpret that
#'   error message as end of iteration.
#' @param sigil If `obj` is a function without an `or` argument, specify
#'   which value to watch for end of iteration. Stop will be signaled
#'   if the function result is [identical()] to `sigil`.
iteror.function <- function(obj, ..., catch, sigil) {
  if ("or" %in% names(formals(obj))) {
    fn <- obj
  } else {
    if (!missing(sigil)) {
      force(sigil)
      fn <- function(or) {
        x <- obj(); if (identical(x, sigil)) or else x
      }
    } else if (!missing(catch)) {
      force(catch)
      fn <- function(or) {
        tryCatch(obj(), error=function(e) {
          if (identical(e, message)) {
            or
          } else stop(e)
        })
      }
    } else {
      stop("iteror: must have 'or' argument or else specify 'catch' or 'sigil'")
    }
  }
  structure(list(nextOr=fn), class=c("iteror", "iter"))
}

#' @exportS3Method
#' @rdname iteror
#' @param recycle a boolean describing whether the iterator should reset after
#' running through all it's values.
#' @param checkFunc a function which, when passed an iterator value, return
#' \code{TRUE} or \code{FALSE}.  If \code{FALSE}, the value is skipped in the
#' iteration.

iteror.default <- function(obj, ...,
                           recycle=FALSE,
                           checkFunc=function(...)(TRUE)) {
  if (is.function(obj)) {
    iteror.function(obj, ...)
  } else {
    i <- 0
    n <- length(obj)
    if (recycle) {
      x <- iteror.function(function(or, ...) {
        repeat {
          i <<- i %% n + 1
          val <- obj[[i]]
          if(checkFunc(val)) return(val)
        }
      }, ...)
    } else {
      x <- iteror.function(function(or, ...) {
        repeat{
          if (i < n) {
            i <<- i + 1
            val <- obj[[i]]
            if(checkFunc(val)) return(val)
          } else return(or)
        }
      }, ...)
    }
    x$length <- n
    x$recycle <- recycle
    x$state <- environment(x$nextOr)
    x
  }
}

#' Retreive the next element from an iteror.
#' @export
#' @param obj An [iteror]
#' @param or If the iterator has reached its end, an argument that
#'   will be forced and returned
nextOr <- function(obj, or, ...) {
  UseMethod("nextOr")
}

#' @exportS3Method
nextOr.iteror <- function(obj, or, ...) {
  obj$nextOr(or, ...)
}

#' @exportS3Method iterators::nextElem iteror
nextElem.iteror <- function(obj, ...) {
  nextOr(obj, stop("StopIteration"), ...)
}

#' Create an iterator that supports the hasNext method
#'
#' \code{ihasNext} is a generic function that indicates if the iterator has
#' another element.
#'
#'
#' @param iterable an iterable object, which could be an iterator.
#' @return An \code{ihasNext} iterator that wraps the specified iterator and
#' supports the \code{hasNext} method.
#' @keywords utilities
#' @examples
#'
#'   it <- ihasNext(c('a', 'b', 'c'))
#'   while (hasNext(it))
#'     print(nextOr(it))
#'
#' @export ihasNext
ihasNext <- function(obj, ...) {
  UseMethod("ihasNext")
}

#' @exportS3Method
ihasNext.ihasNextOr <- identity

#' @exportS3Method
ihasNext.default <- function(obj, ...) ihasNext(iteror(obj, ...))

#' @exportS3Method
nextOr.iter <- function(iter, or, ...) {
  # :( this means that if you use nextOr over a regular iter, you
  # are setting up and tearing down a tryCatch in each iteration...
  tryCatch(
    iterators::nextElem(iter, ...),
    error=function(e)
      if (!identical(conditionMessage(e), 'StopIteration')) stop(e) else or)
}

#' @exportS3Method
nextElem.iteror <- function(iter, ...) {
  nextOr(iter, stop("StopIteration"))
}

#' @exportS3Method as.list iteror
as.list.iteror <- function(x, n=as.integer(2^31-1), ...) {
  size <- 64
  a <- vector('list', length=size)
  i <- 0
  while(i < n) {
    item <- nextOr(x, break)
    i <- i + 1
    if (i >= size) {
      size <- min(2 * size, n)
      length(a) <- size
    }
    a[i] <- list(item)
  }
  length(a) <- i
  a
}

#' \code{is.iteror} indicates if an object is an iterator.
#'
#' @aliases is.iterator
#' @param x any object.
#' @keywords utilities
#' @examples
#'
#' it <- iteror(1:3)
#' stopifnot(is.iteror(it))
#' repeat {
#'   print(nextOr(it, break))
#' }
#'
#' @export is.iteror
is.iteror <- function(x) inherits(x, 'iteror') || inherits(x, 'iter')
