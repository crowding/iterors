#
# Copyright (c) 2014-2015, Stephen B. Weston
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

iarray <- function(X, MARGIN, ..., chunks, chunkSize, drop,
                   idx=lapply(dim(X), function(i) TRUE), quote=FALSE) {
  mcall <- match.call(expand.dots=FALSE)
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

  # Define the "nextElem" function
  nextEl <- if (mlen == 1) {
    if (quote) {
      function() {
        m <- nextElem(it)
        j <- i + m
        q[[iq]] <- if (m > 1) call(':', i + 1, j) else j
        i <<- j
        q
      }
    } else {
      function() {
        m <- nextElem(it)
        j <- i + m
        q[[iq]] <- if (m > 1) call(':', i + 1, j) else j
        i <<- j
        eval(q)
      }
    }
  } else if (! missing(chunks)) {
    function() {
      m <- nextElem(it)
      j <- i + m
      idx[[MARGIN[mlen]]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      mcall$MARGIN <- MARGIN[-mlen]
      mcall$chunks <- chunks[-mlen]
      mcall$drop <- drop
      mcall$idx <- idx
      eval(mcall, parent.frame())
    }
  } else if (! missing(chunkSize)) {
    function() {
      m <- nextElem(it)
      j <- i + m
      idx[[MARGIN[mlen]]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      mcall$MARGIN <- MARGIN[-mlen]
      mcall$chunkSize <- chunkSize[-mlen]
      mcall$drop <- drop
      mcall$idx <- idx
      eval(mcall, parent.frame())
    }
  } else {
    function() {
      nextElem(it)  # returns 1 or throws 'StopIteration'
      i <<- i + 1
      idx[[MARGIN[mlen]]] <- i
      mcall$MARGIN <- MARGIN[-mlen]
      mcall$drop <- drop
      mcall$idx <- idx
      eval(mcall, parent.frame())
    }
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}
