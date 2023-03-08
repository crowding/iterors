#
# Copyright (c) 2014-2015, Stephen B. Weston
# Updated 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
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



#' Create an iterator over an array
#'
#' Create an iterator over an array. It is similar to the \code{iapply}
#' function from the iterators package, but it has special support for nested
#' foreach loops.
#'
#'
#' @param X Array to iterate over.
#' @param MARGIN Vector of subscripts to iterate over.  Note that if the length
#' of \code{MARGIN} is greater than one, the resulting iterator will generate
#' iterators which is particularly useful with nested foreach loops.
#' @param \dots Used to force subsequent arguments to be specified by name.
#' @param chunks Number of elements that the iterator should generate.  This
#' can be a single value or a vector the same length as \code{MARGIN}.  A
#' single value will be recycled for each dimension if \code{MARGIN} has more
#' than one value.
#' @param chunkSize The maximum size Number of elements that the iterator
#' should generate.  This can be a single value or a vector the same length as
#' \code{MARGIN}.  A single value will be recycled for each dimension if
#' \code{MARGIN} has more than one value.
#' @param drop Should dimensions of length 1 be dropped in the generated
#' values?  It defaults to \code{FALSE} if either \code{chunks} or
#' \code{chunkSize} is specified, otherwise to \code{TRUE}.
#' @param idx List of length \code{length(dim(X))} containing indices used to
#' create the expression that generates the iteration values.  The default
#' value contains all \code{TRUE} values, but you can use other values in order
#' to iterate over a subset of \code{X}.  See the example below for ways to do
#' that.
#' @param quote A logical value indicating whether the final iteration values
#' should be quoted or not. This can be useful in a parallel context because it
#' can be much more efficient to send call objects to parallel workers than the
#' objects that they evaluate to.
#' @seealso \code{\link[base]{apply}}, \code{\link[iterators]{iapply}}
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#'   # Iterate over matrices in a 3D array
#'   x <- array(1:24, c(2,3,4))
#'   as.list(iarray(x, 3))
#'
#'   # Iterate over subarrays
#'   as.list(iarray(x, 3, chunks=2))
#'
#'   x <- array(1:64, c(4,4,4))
#'   it <- iarray(x, c(2,3), chunks=c(1,2))
#'   jt <- nextOr(it)
#'   nextOr(jt)
#'   jt <- nextOr(it)
#'   nextOr(jt)
#'
#'   it <- iarray(x, c(2,3), chunks=c(2,2))
#'   jt <- nextOr(it)
#'   nextOr(jt)
#'   nextOr(jt)
#'   jt <- nextOr(it)
#'   nextOr(jt)
#'   nextOr(jt)
#'
#'   # Use idx to iterate over only the middle four columns of a matrix
#'   x <- matrix(1:18, nrow=3, ncol=6)
#'   it <- iarray(x, 1, idx=list(TRUE, quote(2:5)))
#'   nextOr(it)
#'   nextOr(it)
#'   nextOr(it)
#'
#'   # It can also be done using alist:
#'   it <- iarray(x, 1, idx=alist(, 2:5))
#'   nextOr(it)
#'   nextOr(it)
#'   nextOr(it)
#'
#'   x <- matrix(1:1800, nrow=3, ncol=600)
#'   it <- iarray(x, 1, idx=alist(, 10:600), quote=TRUE)
#'   (yq <- nextOr(it))
#'   object.size(yq)
#'   object.size(eval(yq))
#'
#' @export iarray
iarray <- function(X, MARGIN, ..., chunks, chunkSize, drop,
                   idx=lapply(dim(X), function(i) TRUE), quote=FALSE) {
  mcall <- match.call(expand.dots=FALSE)
  ### XXX: do we really need to re-evaluate like this???
  mcallenv <- parent.frame()
  mcall$... <- NULL
  dimx <- dim(X)

  # Verify that X has the dim attribute set and length > 0
  if (length(dimx) == 0)
    stop('dim(X) must have a positive length')

  # Check for unknown arguments
  if (length(list(...)) > 0) {
    nms <- names(list(...))
    if (is.null(nms) || '' %in% nms)
      stop('arguments other than X and MARGIN must be named')
    else
      stop('unused argument(s) ', paste(nms, collapse=', '))
  }

  # Don't allow both chunks and chunkSize
  if (! missing(chunks) && ! missing(chunkSize))
    stop('chunks and chunkSize cannot both be specified')

  # Get the number of values this iterator will return
  i <- 0
  mlen <- length(MARGIN)
  n <- dimx[MARGIN[mlen]]

  # Create an iterator based on chunking
  if (! missing(chunks)) {
    if (length(chunks) != 1 && length(chunks) != mlen)
      stop('length of chunks must be 1 or the same as MARGIN')
    if (missing(drop))
      drop <- FALSE
    if (length(chunks) == 1)
      chunks <- rep(chunks, mlen)
    it <- idiv(n, chunks=chunks[mlen])
  } else if (! missing(chunkSize)) {
    if (length(chunkSize) != 1 && length(chunkSize) != mlen)
      stop('length of chunkSize must be 1 or the same as MARGIN')
    if (missing(drop))
      drop <- FALSE
    if (length(chunkSize) == 1)
      chunkSize <- rep(chunkSize, mlen)
    it <- idiv(n, chunkSize=chunkSize[mlen])
  } else {
    if (missing(drop))
      drop <- TRUE
    it <- irep(1, times=n)
  }

  # Create a call object if this is the final dimension
  if (mlen == 1) {
    q <- if (quote)
      as.call(c(list(as.name('['), substitute(X)), idx, list(drop=drop)))
    else
      as.call(c(list(as.name('['), as.name('X')), idx, list(drop=drop)))
    # iq is the index into q used to modify the appropriate element of idx.
    # Note that in this case, MARGIN contains a single value.
    iq <- MARGIN + 2L
  }

  # Define the "nextOr" function
  nextOr_ <- if (mlen == 1) {
    if (quote) {
      function(or) {
        m <- nextOr(it, return(or))
        j <- i + m
        q[[iq]] <- if (m > 1) call(':', i + 1, j) else j
        i <<- j
        q
      }
    } else {
      function(or) {
        m <- nextOr(it, return(or))
        j <- i + m
        q[[iq]] <- if (m > 1) call(':', i + 1, j) else j
        i <<- j
        eval(q)
      }
    }
  } else if (! missing(chunks)) {
    function(or) {
      m <- nextOr(it, return(or))
      j <- i + m
      idx[[MARGIN[mlen]]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      mcall$MARGIN <- MARGIN[-mlen]
      mcall$chunks <- chunks[-mlen]
      mcall$drop <- drop
      mcall$idx <- idx
      eval(mcall, mcallenv)
    }
  } else if (! missing(chunkSize)) {
    function(or) {
      m <- nextOr(it, return(or))
      j <- i + m
      idx[[MARGIN[mlen]]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      mcall$MARGIN <- MARGIN[-mlen]
      mcall$chunkSize <- chunkSize[-mlen]
      mcall$drop <- drop
      mcall$idx <- idx
      eval(mcall, mcallenv)
    }
  } else {
    function(or) {
      nextOr(it, return(or))  # returns 1 or
      i <<- i + 1
      idx[[MARGIN[mlen]]] <- i
      mcall$MARGIN <- MARGIN[-mlen]
      mcall$drop <- drop
      mcall$idx <- idx
      eval(mcall, mcallenv)
    }
  }

  iteror(nextOr_)
}
