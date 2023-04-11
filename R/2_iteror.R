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


# Iterator Factory Functions
#
# \code{iter} is a generic function used to create iterator objects.
#
#
# @aliases iter iter.default iter.iter iter.matrix iter.data.frame
# iter.function
# @param obj an object from which to generate an iterator.
# @param \dots additional arguments affecting the iterator.
# @return The iterator.
# @keywords methods
# @examples
#
# # a vector iterator
# i1 <- iteror(1:3)
# nextOr(i1)
# nextOr(i1)
# nextOr(i1)
#
# # a data frame iterator by column
# i2 <- iteror(data.frame(x = 1:3, y = 10, z = c("a", "b", "c")))
# nextOr(i2)
# nextOr(i2)
# nextOr(i2)
#
# # a data frame iterator by row
# i3 <- iteror(data.frame(x = 1:3, y = 10), by = "row")
# nextOr(i3)
# nextOr(i3)
# nextOr(i3)
#
# # a function iterator
# i4 <- iteror(function() rnorm(1), sigil=NULL)
# nextOr(i4)
# nextOr(i4)
# nextOr(i4)
#

#' Efficient, compact iteration.
#'
#' To create an iteror, call the constructor `iteror` providing a
#' function or other object as argument. The returned object will
#' support the method [nextOr(obj, or)][nextOr] to extract successive
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
#' ```{R}
#' sum <- 0
#' it <- iteror(iseq(0, 100, 7))
#' repeat {
#'   sum <- sum + nextOr(it, break)
#' }
#' ```
#'
#' Another way to use the "or" argument is to give it a sigil value;
#' that is, a special value that will be interpreted as end of
#' iteration.  If the result of calling `nextOr` is `identical()` to
#' the sigil value you provided, then you know the iterator has
#' ended. In R it is commonplace to use `NULL` or `NA` in the role of
#' a sigil, but that only works until you have an iterator that needs
#' to yield NULL itself. A safer alternative is to use a one-shot
#' sigil value; `new.env()` is a good choice, as it produces an object
#' that by construction is not [identical] to any other object in the
#' R session. This pattern looks like:
#'
#' ```{R}
#' sum <- 0
#' stopped <- new.env()
#' it <- iteror(iseq(0, 100, 7))
#' repeat {
#'   val <- nextOr(it, stopped)
#'   if (identical(val, stopped)) break
#'   sum <- sum + val
#' }
#' ```
#'
#' Note that `iteror` objects are simply function objects with a class
#' attribute attached, and all `nextOr.iteror` does is call the
#' function. So if you were in the mood, you could skip calling
#' `nextOr` through S3 dispatch and call the function directly. If you
#' take this approach, make sure you have called `iteror()` to ensure that
#' you have a true `iteror` object.
#'
#' ```{R}
#' sum <- 0
#' it <- iteror(iseq(0, 100, 7))
#' repeat sum <- sum + it(or=break)
#' ```
#'
#' @export
#' @param obj An object to iterate with. If `obj` is a vector, the
#'   iterator will go over the elements of that vector and you can use
#'   `recycle`.  If `obj` is a function, the function will be called
#'   to compute successive elements. The function should have a
#'   leading argument `or` and behave accordingly (only forcing and
#'   returning `or` to signal end of iteration.)  If you provide a
#'   function that does not have an `or` argument, you will need to
#'   specify either `catch` or `sigil`.
#' @param ... Extra arguments used by some `iteror` methods.
#' @return an object of classes 'iteror' and 'iter'.
iteror <- function(obj, ...) {
  UseMethod("iteror")
}

#' @exportS3Method
iteror.iteror <- function(obj, ...) obj

#' @exportS3Method
iteror.iter <- function(obj, ...) {
  nextOr_ <- function(or) {
    tryCatch(
      iterators::nextElem(obj),
      error=function(e)
        if (identical(conditionMessage(e), 'StopIteration')) or else stop(e))
  }
  iteror.internal(nextOr_)
}

#' @exportS3Method iteror "function"
#' @rdname iteror
#' @param ... Undocumented.
#' @param catch If `obj` does not have `or` argument, specify
#'   e.g. `catch="StopIteration"` to interpret that
#'   error message as end of iteration.
#' @param sigil If `obj` does not have an `or` argument, specify
#'   which value to watch for end of iteration. Stop will be signaled
#'   if the function result is [identical()] to `sigil`.
#' @param count If `obj` does not have an `or` argument, specify
#'   how many times to call it before finishing iteration.
iteror.function <- function(obj, ..., catch, sigil, count) {
  if ("or" %in% names(formals(obj))) {
    fn <- obj
  } else {
    if (!missing(sigil)) {
      force(sigil)
      fn <- function(or) {
        x <- obj()
        if (identical(x, sigil)) or else x
      }
    } else if (!missing(catch)) {
      force(catch)
      fn <- function(or) {
        tryCatch(obj(), error=function(e) {
          if (identical(conditionMessage(e), catch)) {
            or
          } else stop(e)
        })
      }
    } else if (!missing(count)) {
      if (is.finite(count)) {
        fn <- function(or) {
          if (count > 0) {
            count <<- count - 1L
            obj()
          } else or
        }
      } else {
        fn <- function(or) obj()
      }
    } else {
      stop("iteror: function must have an 'or' argument, or else specify one of 'catch', 'sigil' or 'count'")
    }
  }
  iteror.internal(fn, ...)
}

iteror.internal <- function(fn, class=character(0)) {
  structure(fn, class=c(class, "iteror", "iter"))
}

#' @exportS3Method
#' @rdname iteror
#' @param recycle a boolean describing whether the iterator should reset after
#' running through all its values.
#' @param chunks Split the input into this many chunks. Default `NA`, use `chunkSize`.
#' @param chunkSize How many elements (or slices) to include in each chunk.
iteror.default <- count_template(
  input = alist(obj=),
  preamble = alist(
    if (is.function(obj)) return(iteror.function(obj, ...)),
    count <- length(obj)),
  output = function(ix, len) {
    if (missing(len)) {
      substitute(obj[[ix]]) # unboxing!!!
    } else {
      substitute(obj[ix + seq_len(len)]) # uhhh 0 based versus 1-based???
    }
  }
)

#' Retreive the next element from an iteror.
#' @export
#' @param obj An [iteror]
#' @param or If the iterator has reached its end, this argument
#'   will be forced and returned.
#' @param ... Other arguments.
nextOr <- function(obj, or, ...) {
  UseMethod("nextOr")
}

#' @exportS3Method
nextOr.iteror <- function(obj, or, ...) {
  obj(or=or, ...)
}

#' @exportS3Method iterators::nextElem iteror
nextElem.iteror <- function(obj, ...) {
  obj(stop("StopIteration"), ...)
}

#' @exportS3Method
nextOr.iter <- function(obj, or, ...) {
  # :( this means that if you use nextOr over a regular iter, you
  # are setting up and tearing down a tryCatch in each iteration...
  tryCatch(
    iterators::nextElem(obj, ...),
    error=function(e)
      if (!identical(conditionMessage(e), 'StopIteration')) stop(e) else or)
}

#' \code{is.iteror} indicates if an object is an iteror.
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
is.iteror <- function(x) inherits(x, 'iteror')
