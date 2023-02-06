#' An efficient and compact iteration protocol.
#'
#' To create an iteror, call the constructor `iteror` providing either
#' a vector or a function as argument. The returned object will
#' support the method [nextElemOr(obj, or)] to extract successive
#' values.
#'
#' The main method for "iteror" is "nextElemOr" rather than
#' "nextElem". Instead of using exceptions, "nextElemOr" uses a lazily
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
#'   val <- nextElemOr(iter, break)
#'   sum <- sum + val;
#' }
#' ```
#'
#' Another way to use the "or" argument is to give it a sigil value;
#' that is, a special value that will be interpreted as end of
#' iteration.  If the result of calling `nextElemOr` is `identical()`
#' to the sigil value you provided, then you know the iterator has
#' ended. In R it is commonplace to use `NULL` or `NA`, in the role of
#' a sigil, but that only works until you have an iterator that needs
#' to yield NULL. A safer alternative is to use a one-shot sigil
#' value; the result of `new.env()` will work, as it returns a value
#' that by construction is not [identical] to any other object in
#' the R session. This pattern looks like:
#'
#' ```
#' sum <- 0
#' stopped <- new.env()
#' repeat {
#'   val <- nextElemOr(iter, stopped)
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
#'   does not have an `or` argument, you need to specify either `catch`
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
#'   e.g. `catch="StopIteration"` to interpret errors with that
#'   message as end of iteration.
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
