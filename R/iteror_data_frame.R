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

