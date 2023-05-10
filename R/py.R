#' The iteror constructor for a Python object (via the
#' [reticulate][package:reticulate] package) wraps a
#' Python iterable to work with the iteror protocol.
#'
#' @rdname iteror
#' @exportS3Method iteror python.builtin.object
iteror.python.builtin.object <- function(obj, ...) {
  stop_unused(...)
  obj <- reticulate::as_iterator(obj)
  sigil <- function() NULL #unique to this environment

  nextOr_ <- function(or) {
    val <- reticulate::iter_next(obj, sigil)
    if (identical(val, sigil)) or else val
  }

  iteror_internal(nextOr_)
}


#' Wrap an iteror to use with Python code. Requires the
#' `reticulate` package.
#' @param obj An iterable object
#' @param ... Passed along to [iteror].
#' @export
py_iteror <- function(obj, ...) {
  obj <- iteror(obj, ...)
  # can't use the usual trick of a closure or environment
  # (which would be guaranteed unique) so just a string, maybe unique...
  sigil <- format(environment())
  reticulate::py_iterator(function() obj(sigil), sigil)
}
