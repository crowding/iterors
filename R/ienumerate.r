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
ienumerate.default <- count_template(
  input = alist(obj = ),
  preamble=alist(
    count <- length(obj)
  ),
  output = function(ix) substitute(list(index=ix, value=obj[[ix]])),
  output_chunk = function(ix, size) substitute({
    index <- ix + seq_len(size)
    list(index=index, value=obj[index])
  })
)

#' @rdname ienumerate
#' @export
ienum <- ienumerate

#' @exportS3Method
#' @rdname ienumerate
#' @description The `ienumerate` method for arrays allows splitting an
#'   array by arbitrary margins, including by multiple margins. The
#'   `index` element returned will be a vector (or if chunking is used, a
#'   matrix) of indices.
#' @param by Which array margins to iterate over. Can be "row", "col", "cell",
#'   or a vector of numerical indices.
#' @param chunkSize How large a chunk to take along the specified
#'   dimension.
#' @param chunks How many chunks to divide the array into.
#' @param recycle Whether to restart the iterator after finishing the
#'   array.
#' @param drop Whether to drop marginalized dimensions. If chunking is
#'   used, this has no effect.
#' @param rowMajor If TRUE, the first index varies fastest, if FALSE, the last index varies fastest.
#' @author Peter Meilstrup
#' @examples
#' a <- array(1:27, c(3, 3, 3))
#' as.list(ienumerate(a, by=c(1, 2), drop=TRUE))
#' as.list(ienumerate(a, by=c(3), drop=FALSE))
#' as.list(ienumerate(a, by=c(2, 3), chunkSize=7))
ienumerate.array <- count_template(
  input = alist(obj = ),
  options = alist(by=c("cell", "row", "column"), rowMajor=TRUE, drop=FALSE),
  preamble = alist(
    if (is.character(by))
      by <- switch(
        match.arg(by),
        cell=seq_along(dim(obj)),
        row=1L,
        column=2L
      ),
    dim <- dim(obj)[by],
    count <- prod(dim),
    args <- c(alist(obj),
            rep(list(quote(expr=)), length(dim(obj))),
            alist(drop=drop))
  ),
  preamble_single=alist(
    indexer <- arrayIndexer(dim, rowMajor=rowMajor)
  ),
  preamble_chunk=alist(
    indexer <- arrayIndexer(dim, rowMajor=rowMajor, chunk=TRUE)
  ),
  output = function(ix)
    substitute({
      index <- indexer(ix)
      args[by+1] <- index
      list(index=index, value=do.call("[", args))
    }),
  output_chunk = function(ix, size)
    substitute({
      ixes <- indexer(ix, size)
      dim.out <- dim(obj)
      dim.out[by] <- 1
      out <- array(obj[c()], c(prod(dim.out), size))
      dim.out <- c(dim.out, size)
      for (i in seq_len(size)) {
        args[by+1] <- ixes[i,]
        out[,i] <- do.call("[", args)
      }
      dim(out) <- dim.out
      # at this point, if the input array has dim 7, 8, 9,
      # going by dim 2,
      # and we have a chnk size of 5,
      # then our "out" has dim 7, 1, 9, 5,
      # and we want to permute it to 7, 5, 9.
      nd <- length(dim(obj)) + 1
      perm <- seq_len(nd)
      perm[by[1]] <- nd
      perm[nd] <- by[1]
      out <- aperm(out, perm)
      dim(out) <- dim(out)[-nd]
      list(index=ixes, value=out)
    })
  )
