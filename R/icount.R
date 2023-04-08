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
#' @param recycle Whether to restart the count after finishing.
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
icount <- function(count=Inf, recycle=FALSE, ..., chunkSize=1L, chunks) {
  if (!is.numeric(count) || length(count) != 1 || is.na(count))
    stop('count must be a numeric value')
  (function() NULL)(...)

  i <- count
  i[1] <- 0L
  storage.mode(i) <- "integer"

  if (missing(chunks)) {
    if (chunkSize == 1L) {
      if (is.finite(count)) {
        if (recycle) {
          nextOr_ <- function(or) if (i >= count) {i[1] <<- 1; i} else (i <<- i + 1L)
        } else {
          nextOr_ <- function(or) if (i >= count) or else (i <<- i + 1L)
        }
      } else {
        nextOr_ <- function(or) (i <<- i + 1L)
      }
    } else { # chunkSize not 1
      nextOr_ <- function(or) {
        if (i >= count) {
          if (recycle) {
            i[1] <<- 0L
          } else {
            return(or)
          }
        }
        ix <- i + seq_len(min(chunkSize, count-i))
        i[1] <<- i[1] + chunkSize
        ix
      }
    }
  } else { # chunks is given
    chunks <- as.integer(min(chunks))
    chunksLeft <- chunks
    nextOr_ <- function(or) {
      repeat {
        if (chunksLeft <= 0L) {
          if (recycle) {
            chunksLeft <<- chunks
            i[1] <<- 0L
          } else {
            return(or)
          }
        }
        thisChunk <- as.integer(ceiling((count - i)/chunksLeft))
        chunksLeft <<- chunksLeft - 1L
        ix <- i + seq_len(thisChunk)
        i[1] <<- i[1] + thisChunk
        if (thisChunk==0) next
        return(ix)
      }
    }
  }

  iteror.internal(nextOr_)
}

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

#' @rdname icount
#' @param vn A vector of integers.
#' @description `icountn(vn)` takes a vector specifying an array size,
#'   and returns an iterator over array indices. Each returned element
#'   is a vector the same length as vn, with the first index varying fastest.
#'   If vn has a names attribute the output will have the same names.
#' @export
#' @examples
#' as.list(icountn(c(2, 3)))
icountn <- function(vn, recycle=FALSE) {
  iteror.internal(icountn.internal(vn, recycle=recycle))
}

icountn.internal <- function(vn, recycle=FALSE) {
  # amazingly, this recursive algo tests faster than the one
  # using arrayIndex.
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

arrayIndex <- function(i, dim) {
  out <- numeric(length(dim))
  i <- i - 1
  for (index in rev(seq_along(dim))) {
    out[index] <- (i %% dim[index]) + 1L
    i <- i %/% dim[index]
  }
  out
}



#' Dividing Iterator
#'
#' Returns an iterator dividing a value into integer chunks, such that
#' `sum(idiv(n ...)) == floor(n)`
#'
#' @param n The total
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
idiv <- function(n, ..., chunks, chunkSize, recycle=FALSE) {
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

  chunksLeft <- chunks
  nLeft <- n

  nextOr_ <- function(or) {
    if (chunksLeft <= 0 || nLeft <= 0)
      if (recycle) {
        chunksLeft <<- chunks
        nLeft <<- n
      } else {
        return(or)
      }

    m <- ceiling(nLeft / chunksLeft)
    nLeft <<- nLeft - m
    chunksLeft <<- chunksLeft - 1
    m
  }

  iteror.internal(nextOr_, "basicIteror")
}
