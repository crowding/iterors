#' Compute the sum, product, or general reduction of an iterator.
#'
#' `reduce(obj, fun)` applies a 2-argument function `fun` between
#' successive elements of obj. For example if `fun` is `+`,
#' `reduce(it, `+`, init=0)` computes `0 + nextElem(it) +
#' nextElem(it) + nextElem(it) + ...` until the iterator finishes,
#' and returns the final value.
#'
#' @export
#' @param obj an iterable object
#' @param fun A function of as least two arguments.
#' @param ... Extra parameters will be passed to each call to `fun`.
#' @param init A starting value.
#' @return The result of accumulation.
#' @author Peter Meilstrup
#'
#' @examples
#' it <- icount(5)
#' total <- reduce(it, `+`) # sum(1:5)
#'
#' it <- icount(5)
#' reduce(it, paste0, "") # "12345"
#'
#' it <- icount(5)
#' prod <- reduce(it, `*`, init=1) # prod(1:5)
#'
#' # the above is equivalent to:
#' it <- icount(5)
#' total <- 1
#' repeat total <- total * nextOr(it, break)
#'
reduce <- function(obj, fun=`+`, init=0, ...) UseMethod("reduce")

#' @rdname reduce
#' @export
#' @exportS3Method
reduce.iteror <- function(obj, fun=`+`, init=0, ...) {
  obj <- iteror(obj)
  rgs <- formals(args(fun))
  if (!is.function(fun)
      || length(rgs) < 2
      && !("..." %in% names(rgs))) {
    stop("`fun` must be a function of two arguments")
  } else {
    repeat {
      val <- obj(or = break)
      init <- fun(init, val, ...)
    }
  }
  init
}

#' `iaccum(obj)` returns the iterator containing
#' each intermediate result. The default settings
#' produce a cumulative sum.
#' @rdname reduce
#' @examples
#' # triangular numbers: 1, 1+2, 1+2+3, ...
#' take(iaccum(icount()), 10, 'numeric')
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
    init <<- fun(init, val, ...)
  }

  iteror.internal(nextOr_)
}

#' @export
#' @exportS3Method
#' @rdname reduce
sum.iteror <- function(..., na.rm=FALSE) {
  it <- ichain(...)
  if (na.rm) {
    it <- idrop(it, is.na)
  }
  reduce(it)
}

#' @exportS3Method
#' @export
#' @rdname reduce
#' @param na.rm Whether to drop NA values when computing sum or prod.
prod.iteror <- function(..., na.rm=FALSE) {
  it <- ichain(...)
  if (na.rm) {
    it <- idrop(it, is.na)
  }
  reduce(it, `*`, 1)
}
