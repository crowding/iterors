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

# allow a matrix to be iterated over in different ways
#' @exportS3Method
#' @rdname iteror
iteror.matrix <- function(obj, by=c('column', 'cell', 'row'), chunksize=1L,
                          checkFunc=function(...) TRUE, recycle=FALSE, ...) {
  by <- match.arg(by)
  if ((chunksize > 1L) && (by=='cell')) {
    warning("Chunksize greater than 1 not allowed when using by='cell'\n  Setting chunksize=1")
    chunksize <- 1L
  }
  state <- new.env()
  state$i <- 0L
  n <- switch(by, column=ncol(obj), row=nrow(obj), length(obj))

  getIterVal <- function(plus=0L) {
    i <- state$i + plus
    if (i > n)
      stop('SubscriptOutOfBounds', call.=FALSE)
    j <- i + chunksize - 1L
    switch(by,
           column=obj[, i:min(j, n), drop=FALSE],
           row=obj[i:min(j, n), , drop=FALSE],
           obj[[i]])
  }

  nextOr_ <- function(or, ...) {
    delayedAssign("exit", return(or))
    repeat {
      tryCatch({
        if (checkFunc(getIterVal(1L))) {
          state$i <- state$i + chunksize
          return(getIterVal(1L-chunksize))
        }
        state$i <- state$i + chunksize
      }, error=function(e) {
        if (any(nzchar(e$message))) {
          if (identical(e$message, "SubscriptOutOfBounds") ||
                identical(e$message, "attempt to select more than one element")) {
            if (recycle) {
              state$i <- 0L
            }
            else {
              exit
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

  iteror.function(nextOr_)
}

#' @exportS3Method iteror data.frame
#' @rdname iteror
iteror.data.frame <- function(obj, by=c('column', 'row'),
                              ...,
                              checkFunc=function(...) TRUE, recycle=FALSE) {
  by <- match.arg(by)
  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  n <- switch(by, column=length(obj), nrow(obj))

  getIterVal <- function(plus=0L, check=TRUE, ...) {
    i <- state$i + plus
    if (i > n)
      stop('StopIteration', call.=FALSE)
    switch(by,
           column=obj[, i],
           obj[i, ])
  }

  nextOr_ <- function(or, ...) {
    delayedAssign("exit", return(or))
    repeat {
      tryCatch({
        if (checkFunc(getIterVal(1L))) {
          state$i <- state$i + 1L
          return(getIterVal())
        }
        state$i <- state$i + 1L
      }, error=function(e) {
        if (any(nzchar(e$message))) {
          if (identical(e$message, "StopIteration")) {
            if (recycle) {
              state$i <- 0L
            }
            else {
              exit
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

  iteror.function(nextOr_)
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
#' it <- iteror(c("a", "b", "c"))
#' print(nextElem(it))
#' print(nextElem(it))
#' print(nextElem(it))
#'
#' @export nextElem
nextElem <- function(obj, ...) {
  UseMethod('nextElem')
}
