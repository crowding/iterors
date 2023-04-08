#' Iterator that returns the elements of an object along with their indices
#'
#' Constructs an iterator that returns the elements of an object along with each
#' element's indices. Enumeration is useful when looping through an
#' \code{object} and a counter is required.
#'
#' This function is intended to follow the convention used in Python's
#' \code{enumerate} function where the primary difference is that a list is
#' returned instead of Python's \code{tuple} construct.
#'
#' Each call to \code{\link[iterators]{nextElem}} returns a list with two
#' elements:
#' \describe{
#'   \item{index:}{a counter}
#'   \item{value:}{the current value of \code{object}}
#' }
#'
#' \code{ienum} is an alias to \code{ienumerate} to save a few keystrokes.
#'
#' @export
#' @param obj object to return indefinitely.
#' @param ... Undocumented.
#' @return iterator that returns the values of \code{obj} along with the
#' index of the object.
#' @details First appeared in package `iterators2`.
#'
#' @examples
#' set.seed(42)
#' it <- ienumerate(rnorm(5))
#' as.list(it)
#'
#' # Iterates through the columns of the iris data.frame
#' it2 <- ienum(iris)
#' nextOr(it2, NA)
#' nextOr(it2, NA)
#' nextOr(it2, NA)
#' nextOr(it2, NA)
#' nextOr(it2, NA)
#'
#' @export
#' @rdname ienumerate
ienumerate <- function(obj, ...) {
  UseMethod("ienumerate")
}

#' @exportS3Method
ienumerate.iteror <- function(obj, ...) {
  izip(index=icount(), value=obj)
}

#' @exportS3Method
ienumerate.default <- function(obj, ...) {
  ienumerate.iteror(iteror(obj, ...))
}

#' @rdname ienumerate
#' @export
ienum <- ienumerate

#' @exportS3Method
#' @rdname ienumerate
#' @description The `ienumerate` method for arrays allows splitting an
#'   array by arbitrary margins, and by more than one margin. The
#'   `index` element returned will be a vector (or if `chunkSize` > 1, a
#'   matrix) of indices.
#' @param by Which margind to split an array by.
#' @param chunkSize How large a chunk to take along the specified
#'   dimension.
#' @param recycle Whether to restart the iterator after finishing the
#'   array.
#' @param drop Whether to drop marginalized dimensions. Will have no
#'   effect if chunkSize > 1.
#' @author Peter Meilstrup
#' @examples
#' a <- array(1:27, c(3, 3, 3))
#' as.list(ienumerate(a, by=c(1, 2), chunkSize=2, drop=TRUE))
#' as.list(ienumerate(a, chunkSize=7))
ienumerate.array <- function(obj, ...,
                             by=c("cell", "row", "column"),
                             chunkSize=1L,
                             chunks,
                             recycle=FALSE,
                             drop=FALSE) {
  if (is.character(by))
    by <- switch(
      match.arg(by),
      cell=seq_along(dim(obj)),
      row=1,
      column=2
    )

  iter_size <- dim(obj)[by]
  args <- c(alist(obj),
            rep(list(quote(expr=)), length(dim(obj))),
            alist(drop=drop))
  indexit <- icount(prod(iter_size), recycle=recycle)

  if (chunkSize == 1) {
    nextOr_ <- function(or) {
      ix <- nextOr(indexit, return(or))
      args[by+1] <- arrayInd(ix, iter_size)
      list(index=ix, value=do.call("[", args))
    }
  } else {
    indexit <- ichunk(indexit, chunkSize, "numeric")
    nextOr_ <- function(or) {
      ixes <- nextOr(indexit, return(or))
      ixes <- arrayInd(ixes, iter_size)
      dim.out <- c(dim(obj), nrow(ixes))
      dim.out[by] <- 1

      out <- apply(ixes, 1, function(ix) {
        args[by+1] <- ix
        do.call("[", args)
      }, simplify=FALSE)
      out <- do.call(c, out)
      dim(out) <- dim.out
      # if we have by[1]==2, aperm=1,4,3
      nd <- length(dim(obj)) + 1
      perm <- seq_len(nd)
      perm[by[1]] <- nd
      perm[nd] <- by[1]
      out <- aperm(out, perm)
      dim(out) <- dim(out)[-nd]
      list(index=ixes, value=out)
    }
  }

  iteror.internal(nextOr_, "basicIteror")
}
