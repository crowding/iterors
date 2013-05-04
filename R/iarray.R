iarray <- function (X, MARGIN, ...) {
  i <- 0L
  xdim <- dim(X)
  mdim <- xdim[MARGIN]
  n <- prod(mdim)
  q <- as.call(c(list(as.name('['), as.name('X')),
                 as.list(rep(TRUE, length(xdim)))))
  iq <- MARGIN + 2L

  nextEl <- function() {
    if (i >= n)
      stop('StopIteration', call.=FALSE)
    i <<- i + 1L
    q[iq] <- arrayInd(i, .dim=mdim)
    eval(q)
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}
