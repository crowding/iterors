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
#' @param count number of times that the iterator will fire. Use NA or
#'   Inf to make an iterator that counts forever.
#' @param ... Undocumented
#' @param recycle Whether to restart the count after finishing.
#' @param chunkSize How many valies to return from each call to nextOr().
#' @param chunks How many chunks to split the input. Either `chunks` or `chunkSize` may be given but not both.
#' @return The counting iterator.
#' @keywords utilities
#' @details Originally from the `iterators` package.
#' @seealso For more control over starting number and step size, see
#'   [iseq].
#' @examples
#'
#' # create an iterator that counts from 1 to 3.
#' it <- icount(3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' @export
#' @examples
#' x <- icount(5)
#' repeat print(nextOr(x, break))
icount <- count_template(input=alist(count=Inf),
                         output=function(ix, size) {
                           if (missing(size)) substitute(ix)
                           else substitute(ix + seq_len(size))
                         })

icount.internal <- function(n, recycle=FALSE) {
  x <- n
  x[1] <- 0
  storage.mode(x) <- "integer"
  if (recycle) {
    function(or) if (x >= n) (x[1] <<- 1L) else (x <<- x + 1L)
  } else {
    function(or) if (x >= n) or else (x <<- x + 1L)
  }
}

#' Dividing Iterator
#'
#' Returns an iterator dividing a value into integer chunks, such that
#' `sum(idiv(n, ...)) == floor(n)`
#'
#' @param count The total
#' @param ... Unused.
#' @param recycle Whether to restart the count after finishing.
#' @param chunkSize the maximum size of the pieces that \code{n} should be
#' divided into.  This is useful when you know the size of the pieces that you
#' want.  If specified, then \code{chunks} should not be.
#' @param chunks the number of pieces that \code{n} should be divided into.
#' This is useful when you know the number of pieces that you want.  If
#' specified, then \code{chunkSize} should not be.
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
idiv <- count_template(input=alist(count=),
                       output=function(ix, size)
                         if (missing(size)) 1 else substitute(size))

#' @rdname icount
#' @param vn A vector of integers.
#' @param rowMajor If `TRUE` (default), the earliest indices will cycle fastest;
#'   if `FALSE`, last indices cycle fastest.
#' @description `icountn(vn)` takes a vector specifying an array size,
#'   and returns an iterator over array indices. Each returned element
#'   is a vector the same length as vn, with the first index varying fastest.
#'   If vn has a names attribute the output will have the same names.
#' @export
#' @examples
#' as.list(icountn(c(2, 3)))
icountn <- count_template(
  input = alist(vn = ),
  options = alist(rowMajor = TRUE),
  preamble=alist(
    storage.mode(vn) <- "integer",
    count <- prod(vn)
  ),
  output = function(ix, size) {
    if (missing(size))
      substitute(arrayIndex(ix, vn, rowMajor))
    else
      substitute(arrayIndices(ix + seq_len(size), vn, rowMajor))
  })

icountn.recursive <- function(vn, recycle = FALSE) {
  iteror.internal(icountn.internal(vn, recycle = recycle))
}

icountn.internal <- function(vn, recycle = FALSE) {
  # amazingly, this recursive algo tests faster than the one
  # using arrayIndex (icountn.simple)
  if (length(vn) > 1) {
    icar <- NULL
    icdr <- NULL
    carVal <- NULL
    nextOr_ <- function(or) {
      if (is.null(icar)) {
        icar <<- icountn.internal(vn[-1])
        carVal <<- icar(return(or))
        icdr <<- icount.internal(vn[1])
      }
      cdr <- icdr({
        carVal <<- icar(return(or))
        icdr <<- icount.internal(vn[1])
        icdr(return(or))
      })
      c(cdr, carVal)
    }
  } else if (length(vn) == 1) {
    return(icount.internal(vn, recycle=recycle))
  } else {
    nextOr_ <- function(or) or
  }
  nextOr_
}

icountn.simple <- function(vn, recycle=FALSE) {
  iapply(icount(prod(vn), recycle=recycle),
         function(i) as.vector(arrayIndex(i, vn)))
}

arrayIndex <- function(i, dim, rowMajor=FALSE) {
  out <- dim
  i <- i - 1L
  for (index in if (rowMajor) seq_along(dim) else rev(seq_along(dim))) {
    out[index] <- (i %% dim[index]) + 1L
    i <- i %/% dim[index]
  }
  out
}

arrayIndices <- function(i, dim, rowMajor=FALSE) {
  out <- structure(integer(length(i) * length(dim)),
                   dim=c(length(i), length(dim)),
                   dimnames={
                     if (is.null(names(dim))) NULL
                     else list(NULL, names(dim))
                   })
  i <- i - 1L
  for (index in if (rowMajor) seq_along(dim) else rev(seq_along(dim))) {
    coord <- (i %% dim[index]) + 1L
    out[,index] <- coord
    i <- i %/% dim[index]
  }
  out
}
