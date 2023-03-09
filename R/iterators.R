#
# Copyright (c) 2008-2010 Revolution Analytics
# Updated 2023 by Peter Meilstrup
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

# allow a matrix to be iterated over in different ways

#' @rdname iteror
#' @param by how to iterate over a matrix.
#' @param chunksize the number of elements of \code{by} to return with each
#' call to \code{nextElem}.
#' @exportS3Method
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

#' @rdname iteror
#' @exportS3Method
iteror.array <- iteror.matrix

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
