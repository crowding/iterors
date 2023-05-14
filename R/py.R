#' @rdname iteror
#' @return The method for class `python.builtin.object` wraps
#'   a Python iterable to work as an R iteror. This requires the
#'   `reticulate` package to be installed.
#' @exportS3Method iteror python.builtin.object
#' @examples
#' pit <- py_eval("(n for n in range(1, 25) if n % 3 == 0)")
#' sum(iteror(pit))
iteror.python.builtin.object <- function(obj, ...) {
  stop_unused(...)
  obj <- reticulate::as_iterator(obj)
  signal <- function() NULL

  nextOr_ <- function(or) {
    val <- reticulate::iter_next(obj, signal)
    if (identical(val, signal)) or else val
  }

  iteror_internal(nextOr_)
}


#' Wrap an iteror to pass to Python functions, via the
#' `reticulate` package.
#' @param x An iterable object.
#' @param ... Passed along to [iteror(x, ...)].
#' @return a Python iterator.
#' @export
#' @exportS3Method reticulate::r_to_py iteror
#' @examples
#' pit <- r_to_py(iseq(2, 11, 5))
#' reticulate::iter_next(pit, NULL)
#' reticulate::iter_next(pit, NULL)
#' reticulate::iter_next(pit, NULL)
#'
#' triangulars <- icount() |> i_accum() |> i_limit(10)
#' builtins <- reticulate::import_builtins()
#' builtins$sum(triangulars) # r_to_py is called automatically
r_to_py.iteror <- function(x, convert=FALSE, ...) {
  x <- iteror(x, ...)
  signal <- r_to_py(function() NULL)
  reticulate::py_iterator(function() x(signal), signal)
}
