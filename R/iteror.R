#' An efficient and succinct iteration protocol
#'
#' The "iterators" package uses stop("StopIteration") and tryCatch to
#' signal end of iteration, but tryCatch has a not-insiginificant
#' amount of overhead. In the context of a generator, when you are in a
#' "for" loop over an iterator, you have to be setting up and tearing
#' .down the trycatch on each iteration. so that you can return control
#' from the generator.
#'
#' The main method for "iteror" is "nextElemOr" rather than
#' "nextElem". Instead of exceptions, "nextElemOr" uses a lazily
#' evaluated "or" argument to signal the end of iteration.  The "or"
#' argument is lazily evaluated, and will only be forced at the stop of
#' iteration; this means the consumer can provide a "break" or "return"
#' to respond to the end of the loop.
#'
#' ```
#' sum <- 0
#' it <- iterors::iteror(in)
#' repeat {
#'   val <- nextElemOr(iter, break)
#'   sum <- sum + val;
#' }
#' ```
#'
#' Another way to use the "or" argument is to give it a sigil value;
#' that is, a value that you know will not appear in the values
#' yielded by the generator. If the result of `nextElemOr` is this sigil
#' value, then you know the iterator has ended. In R it is commonplace
#' to use `NULL` or NA, as a sigil, but you do sometimes want to have
#' an iterator return those values literally. A generally safer
#' pattern is to use a one-shot sigil value; the result of `new.env()`
#' will work, as it returns a value that by construction is not
#' [identical]() to any other object in the R session.
#'
#' ```
#' stopped <- new.env()
#' sum <- 0
#' repeat {
#'   i <- nextElemOr(iter, stopped)
#'   if (identical(i, stopped)) break
#'   sum <- sum + i
#' }
#' ```
#'
#' @export
#' @param obj An object to turn into an iteror. If it is a function
#'   with a leading argument named `or` this is turned directly into
#'   an iterator. Otherwise you must specify. If `obj` is a vector the
#'   iterator will go over the elements of that vector
iteror <- function(obj, ...) {
  UseMethod("iteror")
}

#' @exportS3Method
iteror.iteror <- identity

#' @exportS3Method
iteror.iter <- identity

#' @exportS3Method iteror "function"
#' @rdname iteror
#' @param catch If `obj` is a function with no arguments, specify
#'   e.g. `catch="stopIteration"` to interpret errors with that
#'   message as end of iteration.
#' @param sigil If `obj` is a function with no arguments, specify
#'   which value to watch for end of iteration. Stop will be signaled
#'   if the function result is [identical]() to `sigil`.
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
  structure(list(nextElemOr=fn), class=c("funiteror", "iteror", "iter"))
}

#' @exportS3Method
#' @rdname iteror
#' @param recycle If `obj` is a vector, and `recycle` is TRUE, the
#'   iterator will re-cycle the elements of `obj` without stopping.
iteror.default <- function(obj, ..., recycle=FALSE) {
  if (is.function(obj)) {
    iteror.function(obj, ...)
  } else {
    i <- 0
    n <- length(obj)
    if (recycle) {
      x <- iteror.function(function(or, ...) {
        i <<- i %% n + 1
        obj[[i]]
      }, ...)
    } else {
      x <- iteror.function(function(or, ...) {
        if (i < n) {
          i <<- i + 1
          obj[[i]]
        } else or
      }, ...)
    }
    x$length <- n
    x$recycle <- recycle
    x$state <- environment(x$nextElemOr)
    x
  }
}

#' Retreive the next element from an iteror.
#' @export
#' @param obj An [iteror]
#' @param or If the iterator has reached its end, an argument that
#'   will be forced and returned
nextElemOr <- function(obj, or, ...) {
  UseMethod("nextElemOr")
}

#' @exportS3Method
nextElemOr.funiteror <- function(obj, or, ...) {
  obj$nextElemOr(or, ...)
}

# @exportS3Method iterators::nextElem
nextElem.iteror <- function(obj, ...) {
  nextElemOr(obj, stop("StopIteration"), ...)
}

#' @export
ihasNext <- function(obj, ...) {
  UseMethod("ihasNext")
}

#' @exportS3Method
ihasNext.ihasNextOr <- identity

#' @exportS3Method
ihasNext.default <- function(obj) ihasNext(iteror(obj))

#' @exportS3Method
nextElemOr.iter <- function(iter, or) {
  # :( this means that if you use nextElemOr over a regular iter, you
  # are setting up and tearing down a tryCatch in each iteration...
  tryCatch(
    nextElem(iter),
    error=function(e)
      if (!identical(conditionMessage(e), 'StopIteration')) stop(e) else or)
}

#' @exportS3Method
ihasNext.iteror <- function(iter, ...) {
  noValue <- new.env()
  endIter <- new.env()
  last <- noValue
  structure(function(or, query="next", ...) {
    switch(query,
           "next"={
             if (identical(last, noValue))
               last <<- nextElemOr(iter, endIter)
             if (identical(last, endIter))
               or
             else
               last %then% (last <<- noValue)
           },
           "has"={
             if (identical(last, noValue))
               last <<- nextElemOr(iter, endIter)
             !identical(last, endIter)
           },
           stop("unknown query: ", query)
           )
  }, class=c("ihasNextOr", "iteror", "ihasNext", "iter"))
}

#' @exportS3Method
nextElem.ihasNextOr <- function(obj, ...) {
  obj(stop("StopIteration", call.=FALSE), query="next", ...)
}

#' @exportS3Method
nextElemOr.ihasNextOr <- function(obj, or, ...) {
  obj(or, query="next", ...)
}

#' @exportS3Method
hasNext.ihasNextOr <- function(obj, ...) {
  obj(query="has", ...)
}

#' @export
hasNext <- function(obj, ...) {
  UseMethod("hasNext")
}

#' @exportS3Method as.list iteror
as.list.iteror <- function(x, n=as.integer(2^31-1), ...) {
  size <- 64
  a <- vector('list', length=size)
  i <- 0
  repeat {
    item <- nextElemOr(x, break)
    i <- i + 1
    if (i >= size) {
      size <- min(2 * size, n)
      length(a) <- size
    }
    a[[i]] <- item
  }
  length(a) <- i
  a
}
