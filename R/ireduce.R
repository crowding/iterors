#' Compute the sum, product, or general reduction of an iterator.
#'
#' @export
#' @param obj an iterable object
#' @param fun A function of as least two arguments.
#' @param ... Extra parameters to be passed to `f`
#' @param init A starting value.
#' @return The result of accumulation.
#' @author Peter Meilstrup
#'
#' @examples
#' it <- icount(5)
#' ireduce(it, `+`) # sum(1:5)
#'
#' it <- icount(5)
#' ireduce(it, paste0, "") # "12345"
#'
#' it <- icount(5)
#' ireduce(it, `*`, 1) # prod(1:5)
#'
ireduce <- function(obj, fun=`+`, init=0, ...) {
  obj <- iteror(obj)
  rgs <- formals(args(fun))
  if (!is.function(fun)
      || length(rgs) < 2
      && !("..." %in% names(rgs))) {
    stop("`fun` must be a function of two arguments")
  } else {
    repeat {
      val <- nextOr(obj, break)
      init <- fun(init, val)
    }
  }
  init
}

#' `iaccum(obj)` returns the iterator containing
#' each intermediate result. The default settings
#' produce a cumulative sum.
#' @rdname ireduce
#' @export
iaccum <- function(obj, fun=`+`, init=0, ...) {
  obj <- iteror(obj)

  rgs <- formals(args(fun))
  if (!is.function(fun)
      || length(rgs) < 2
      && !("..." %in% names(rgs))) {
    stop("`fun` must be a function of two arguments")
  }

  nextOr_ <- function(or) {
    val <- nextOr(obj, return(or))
    init <<- fun(init, val)
  }

  iteror.function(nextOr_)
}

#' @exportS3Method
#' @rdname ireduce
sum.iteror <- function(..., na.rm) {
  it <- ichain(...)
  if (na.rm) {
    it <- idrop(it, is.na)
  }
  ireduce(it)
}

#' @exportS3Method
#' @rdname ireduce
#' @param na.rm Whether to drop NA values when computing sum or prod.
prod.iteror <- function(..., na.rm) {
  it <- ichain(...)
  if (na.rm) {
    it <- idrop(it, is.na)
  }
  ireduce(it, `*`, 1)
}
