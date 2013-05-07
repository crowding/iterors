iarray <- function(X, MARGIN, ..., chunks, chunkSize, drop,
                   idx=as.list(rep(TRUE, length(dim(X))))) {
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

  # Get the number of value this iterator will return
  i <- 0
  n <- dim(X)[MARGIN[1]]

  # Create an iterator based on chunking
  if (! missing(chunks)) {
    if (length(chunks) != 1 && length(chunks) != length(MARGIN))
      stop('length of chunks must be 1 or the same as MARGIN')
    if (missing(drop))
      drop <- FALSE
    if (length(chunks) == 1)
      chunks <- rep(chunks, length(MARGIN))
    it <- idiv(n, chunks=chunks[1])
  } else if (! missing(chunkSize)) {
    if (length(chunkSize) != 1 && length(chunkSize) != length(MARGIN))
      stop('length of chunkSize must be 1 or the same as MARGIN')
    if (missing(drop))
      drop <- FALSE
    if (length(chunkSize) == 1)
      chunkSize <- rep(chunkSize, length(MARGIN))
    it <- idiv(n, chunkSize=chunkSize[1])
  } else {
    if (missing(drop))
      drop <- TRUE
    it <- irep(1, times=n)
  }

  # Create a call object if this is the final dimension
  if (length(MARGIN) == 1) {
    q <- as.call(c(list(as.name('['), as.name('X')), idx, list(drop=drop)))
    iq <- MARGIN + 2L
  }

  # Define the "nextElem" function
  nextEl <- if (length(MARGIN) == 1) {
    function() {
      m <- nextElem(it)
      j <- i + m
      q[[iq]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      eval(q)
    }
  } else if (! missing(chunks)) {
    function() {
      m <- nextElem(it)
      j <- i + m
      idx[[MARGIN[1]]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      iarray(X, MARGIN[-1], chunks=chunks[-1], drop=drop, idx=idx)
    }
  } else if (! missing(chunkSize)) {
    function() {
      m <- nextElem(it)
      j <- i + m
      idx[[MARGIN[1]]] <- if (m > 1) call(':', i + 1, j) else j
      i <<- j
      iarray(X, MARGIN[-1], chunkSize=chunkSize[-1], drop=drop, idx=idx)
    }
  } else {
    function() {
      nextElem(it)  # returns 1 or throws 'StopIteration'
      i <<- i + 1
      idx[[MARGIN[1]]] <- i
      iarray(X, MARGIN[-1], drop=drop, idx=idx)
    }
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}
