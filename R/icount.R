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

#' Counting Iterators
#'
#' Returns an iterator that counts starting from one.
#'
#' @aliases icount
#' @param count number of times that the iterator will fire.  If not specified,
#' it will count forever.
#' @param recycle Whether to restart the count after finishing.
#' @return The counting iterator.
#' @keywords utilities
#' @details Originally from the `iterators` package.
#' @seealso For more control over starting number and step size, see [iseq].
#' @examples
#'
#' # create an iterator that counts from 1 to 3.
#' it <- icount(3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' @export icount icountn
#' @examples
#' x <- icount(5)
#' repeat print(nextOr(x, break))
icount <- function(count, recycle=FALSE) {
  if (missing(count))
    count <- NULL
  else if (!is.numeric(count) || length(count) != 1 || is.na(count))
    stop('count must be a numeric value')

  i <- 0L

  if (is.null(count))
    nextOr_ <- function(or) {
      (i <<- i + 1L)
    }
  else
    nextOr_ <- function(or) {
      if (i < count)
        (i <<- i + 1L)
      else if (recycle)
        i <<- 1L
      else
        or
    }

  iteror.function(nextOr_)
}

#' @rdname icount
#' @description `icountn(vn)` takes a vector specifying an array size,
#'   and returns an iterator over array indices. Each returned element
#'   is a vector the same length as vn, with the first index varying fastest.
#' @export icountn
#' @examples
#' as.list(icountn(c(2, 3)))
icountn <- function(vn, recycle=FALSE) {
  iapply(icount(prod(vn), recycle=recycle),
         function(i) as.vector(arrayInd(i, vn)))
}

#' Dividing Iterator
#'
#' Returns an iterator that returns pieces of numeric value.
#'
#'
#' @param n number of times that the iterator will fire.  If not specified, it
#' will count forever.
#' @param \dots unused.
#' @param chunks the number of pieces that \code{n} should be divided into.
#' This is useful when you know the number of pieces that you want.  If
#' specified, then \code{chunkSize} should not be.
#' @param chunkSize the maximum size of the pieces that \code{n} should be
#' divided into.  This is useful when you know the size of the pieces that you
#' want.  If specified, then \code{chunks} should not be.
#' @return The dividing iterator.
#' @keywords utilities
#' @details Originally from the `iterators` package.
#' @examples
#'
#' # divide the value 10 into 3 pieces
#' it <- idiv(10, chunks = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' # divide the value 10 into pieces no larger than 3
#' it <- idiv(10, chunkSize = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # end of iterator
#'
#' @export idiv
idiv <- function(n, ..., chunks, chunkSize) {
  if (!is.numeric(n) || length(n) != 1)
    stop('n must be a numeric value')

  if (length(list(...)) > 0)
    stop('chunks and chunkSize must be specified as named arguments')

  if ((missing(chunkSize) && missing(chunks)) ||
      (!missing(chunkSize) && !missing(chunks)))
    stop('either chunks or chunkSize must be specified, but not both')

  if (missing(chunks)) {
    if (!is.numeric(chunkSize) || length(chunkSize) != 1 || chunkSize < 1)
      stop('chunkSize must be a numeric value >= 1')
    chunks <- ceiling(n / chunkSize)
  }

  nextOr_ <- function(or) {
    if (chunks <= 0 || n <= 0)
      return(or)

    m <- ceiling(n / chunks)
    n <<- n - m
    chunks <<- chunks - 1
    m
  }

  iteror.function(nextOr_)
}

