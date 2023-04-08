#' @rdname iteror
#' @param by how to iterate over a matrix Can be "cell", "row", "col", or
#' numeric dimensions.
#' @param chunkSize the number of elements of \code{by} to return with each
#' call to \code{nextElem}.
#' @param drop Whether to drop the array dimensions enumerated over.
#' @examples
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
iteror.array <- function(obj, ...,
                         by=c("cell", "row", "column"),
                         chunkSize=1L,
                         recycle=FALSE,
                         drop=FALSE) {

  if (is.character(by) && match.arg(by) == "cell") {
    it <- iteror.default(obj, chunksize=chunksize, recycle=recycle)
  } else {
    it <- iapply(ienumerate.array(obj, by=by,
                                  chunksize=chunksize, recycle=recycle, drop=drop),
                 function(x)x$value)
  }
  it
}

#' @rdname iteror
#' @exportS3Method
iteror.matrix <- iteror.array
