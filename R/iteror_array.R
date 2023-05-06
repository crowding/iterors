#' @rdname iteror
#' @param by how to split up an array or data frame. Can be "cell",
#'   "row", "column", or numeric dimensions.
#' @param chunkSize the number of elements of \code{by} to return with
#'   each call to \code{nextOr}.
#' @param drop Whether to drop the array dimensions enumerated over.
#' @param rowMajor If TRUE, will return slices in order with the first
#'              indices varying fastest (same as in [ienumerate]).
#' @examples
#' l <- iteror(letters, chunkSize=7)
#' as.list(l)
#'
#' a <- array(1:8, c(2, 2, 2))
#'
#' # iterate over all the slices
#' it <- iteror(a, by=3)
#' as.list(it)
#'
#' # iterate over all the columns of each slice
#' it <- iteror(a, by=c(2, 3))
#' as.list(it)
#'
#' # iterate over all the rows of each slice
#' it <- iteror(a, by=c(1, 3))
#' as.list(it)
#'
#' @exportS3Method
#' @export
iteror.array <- function(obj, ...,
                         by=c("cell", "row", "column"),
                         chunkSize,
                         chunks,
                         recycle=FALSE,
                         drop=FALSE,
                         rowMajor=TRUE) {

  if (is.character(by) && match.arg(by) == "cell" && rowMajor) {
    it <- iteror.default(obj, ...,
                         chunkSize=chunkSize, chunks=chunks,
                         recycle=recycle)
  } else {
    it <- iapply(ienumerate.array(obj, ..., by=by,
                                  chunkSize=chunkSize, chunks=chunks,
                                  recycle=recycle, drop=drop, rowMajor=rowMajor),
                 function(x)x$value)
  }
  it
}

#' @rdname iteror
#' @exportS3Method
iteror.matrix <- iteror.array
