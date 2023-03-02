#
# Copyright (c) 2008-2010 Revolution Analytics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# generic function for creating an iterator object


#' Iterator Factory Functions
#'
#' \code{iter} is a generic function used to create iterator objects.
#'
#'
#' @aliases iter iter.default iter.iter iter.matrix iter.data.frame
#' iter.function
#' @param obj an object from which to generate an iterator.
#' @param by how to iterate.
#' @param chunksize the number of elements of \code{by} to return with each
#' call to \code{nextElem}.
#' @param checkFunc a function which, when passed an iterator value, return
#' \code{TRUE} or \code{FALSE}.  If \code{FALSE}, the value is skipped in the
#' iteration.
#' @param recycle a boolean describing whether the iterator should reset after
#' running through all it's values.
#' @param \dots additional arguments affecting the iterator.
#' @return The iterator.
#' @keywords methods
#' @examples
#'
#' # a vector iterator
#' i1 <- iter(1:3)
#' nextElem(i1)
#' nextElem(i1)
#' nextElem(i1)
#'
#' # a vector iterator with a checkFunc
#' i1 <- iter(1:3, checkFunc = function(i) i%%2 == 0)
#' nextElem(i1)
#'
#' # a data frame iterator by column
#' i2 <- iter(data.frame(x = 1:3, y = 10, z = c("a", "b", "c")))
#' nextElem(i2)
#' nextElem(i2)
#' nextElem(i2)
#'
#' # a data frame iterator by row
#' i3 <- iter(data.frame(x = 1:3, y = 10), by = "row")
#' nextElem(i3)
#' nextElem(i3)
#' nextElem(i3)
#'
#' # a function iterator
#' i4 <- iter(function() rnorm(1))
#' nextElem(i4)
#' nextElem(i4)
#' nextElem(i4)
#'
#' @export iter
iter <- function(obj, ...) {
  UseMethod('iter')
}

# calling iter on an iter object returns itself
#' @exportS3Method
iter.iter <- function(obj, ...) {
  obj
}

# default method creates an iterator from a vector or list
#' @exportS3Method
iter.default <- function(obj, checkFunc=function(...) TRUE, recycle=FALSE, ...) {
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  n <- length(obj)
  it <- list(state=state, length=n, checkFunc=checkFunc, recycle=recycle)
  class(it) <- c('containeriter', 'iter')
  it
}

# allow a matrix to be iterated over in different ways
#' @exportS3Method
iter.matrix <- function(obj, by=c('column', 'cell', 'row'), chunksize=1L,
                        checkFunc=function(...) TRUE, recycle=FALSE, ...) {
  by <- match.arg(by)
  if ((chunksize > 1L) && (by=='cell')) {
    warning("Chunksize greater than 1 not allowed when using by='cell'\n  Setting chunksize=1")
    chunksize <- 1L
  }
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  n <- switch(by, column=ncol(obj), row=nrow(obj), length(obj))
  it <- list(state=state, by=by, length=n, checkFunc=checkFunc,
             recycle=recycle, chunksize=chunksize)
  class(it) <- c('matrixiter', 'iter')
  it
}

# allow a data frame to be iterated over in different ways
#' @exportS3Method iter data.frame
iter.data.frame <- function(obj, by=c('column', 'row'),
                            checkFunc=function(...) TRUE, recycle=FALSE, ...) {
  by <- match.arg(by)
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  n <- switch(by, column=length(obj), nrow(obj))
  it <- list(state=state, by=by, length=n, checkFunc=checkFunc,
             recycle=recycle)
  class(it) <- c('dataframeiter', 'iter')
  it
}

# allow a closure to be turned into an iterator object
#' @exportS3Method iter "function"
iter.function <- function(obj, checkFunc=function(...) TRUE,
                          recycle=FALSE, ...) {
  state <- new.env()
  state$i <- 0L
  state$fun <- obj
  args <- !is.null(formals(obj))
  it <- list(state=state, args=args, checkFunc=checkFunc,
             recycle=recycle)
  class(it) <- c('funiter', 'iter')
  it
}

getIterVal <- function(obj, plus, ...) {
  UseMethod('getIterVal')
}

#' @exportS3Method
getIterVal.containeriter <- function(obj, plus=0L, ...) {
  i <- obj$state$i + plus
  if (i > obj$length)
    stop('SubscriptOutOfBounds', call.=FALSE)
  obj$state$obj[[i]]
}

#' @exportS3Method
getIterVal.matrixiter <- function(obj, plus=0L, ...) {
  i <- obj$state$i + plus
  n <- obj$length
  if (i > n)
    stop('SubscriptOutOfBounds', call.=FALSE)
  j <- i + obj$chunksize - 1L
  switch(obj$by,
         column=obj$state$obj[, i:min(j, n), drop=FALSE],
         row=obj$state$obj[i:min(j, n), , drop=FALSE],
         obj$state$obj[[i]])
}

#' @exportS3Method
getIterVal.dataframeiter <- function(obj, plus=0L, check=TRUE, ...) {
  i <- obj$state$i + plus
  n <- obj$length
  if (i > n)
    stop('StopIteration', call.=FALSE)
  switch(obj$by,
         column=obj$state$obj[, i],
         obj$state$obj[i, ])
}



#' Get Next Element of Iterator
#'
#' \code{nextElem} is a generic function used to produce values. If a
#' \code{checkFunc} was specified to the constructor, the potential iterated
#' values will be passed to the \code{checkFunc} until the \code{checkFunc}
#' returns \code{TRUE}. When the iterator has no more values, it calls stop
#' with the message 'StopIteration'.
#'
#'
#' @aliases nextElem nextElem.containeriter nextElem.funiter
#' @param obj an iterator object.
#' @param \dots additional arguments that are ignored.
#' @return The value.
#' @keywords methods
#' @examples
#'
#' it <- iter(c("a", "b", "c"))
#' print(nextElem(it))
#' print(nextElem(it))
#' print(nextElem(it))
#'
#' @export nextElem
nextElem <- function(obj, ...) {
  UseMethod('nextElem')
}

#' @exportS3Method
nextElem.containeriter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$i <- obj$state$i + 1L
        return(getIterVal(obj))
      }
      obj$state$i <- obj$state$i + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "SubscriptOutOfBounds")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

#' @exportS3Method
nextElem.matrixiter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$i <- obj$state$i + obj$chunksize
        return(getIterVal(obj,plus=(1L-obj$chunksize)))
      }
      obj$state$i <- obj$state$i + obj$chunksize
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "SubscriptOutOfBounds") ||
            identical(e$message, "attempt to select more than one element")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

#' @exportS3Method
nextElem.dataframeiter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$i <- obj$state$i + 1L
        return(getIterVal(obj))
      }
      obj$state$i <- obj$state$i + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "StopIteration")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

#' @exportS3Method
nextElem.funiter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$args) {
        val <- obj$state$fun(obj$state$i+1L)
      }
      else {
        val <- obj$state$fun()
      }
      if (obj$checkFunc(val)) {
        if (obj$args) obj$state$i <- obj$state$i + 1L
        return(val)
      }
      if (obj$args) obj$state$i <- obj$state$i + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "StopIteration")) {
          if (obj$recycle) {
            if (obj$args) obj$state$i <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}

#' @exportS3Method
nextElem.abstractiter <- function(obj, ...) {
  obj$nextElem()
}

#print.containeriter <- function(x, ...) {
#  repr <- sprintf('<%s iterator, current value %d\n',
#                  class(x$state$obj)[1], getIterVal(x))
#  cat(repr)
#}

#print.matrixiter <- function(x, ...) {
#  repr <- sprintf('<%s iterator, current value %d\n',
#                  class(x$state$obj)[1], getIterVal(x))
#  cat(repr)
#}

#print.dataframeiter <- function(x, ...) {
#  repr <- sprintf('<%s iterator, current value %d\n',
#                  class(x$state$obj)[1], getIterVal(x, check=FALSE))
#  cat(repr)
#}

#print.funiter <- function(x, ...) {
#  cat('function iterator\n')
#}

#print.abstractiter <- function(x, ...) {
#  cat(x$toString())
#}
