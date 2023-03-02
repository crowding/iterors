
#' Helper function that determines the length of an iterator object
#'
#' Returns the length of an iterator object. In the case that the iterator's
#' length is \code{NULL}, a value of 1 is returned by default. This value can be
#' set using the \code{default} argument.
#'
#' @param object an iterator object
#' @param default the value returned when an iterator has \code{NULL} length
#' @return integer
iter_length <- function(object, default=1) {
  ifelse(is.null(object$length), default, object$length)
}

#' Helper function that determines whether is an iteror object
#'
#' Returns \code{TRUE} if the \code{object} is an object of class \code{iteror},
#' and \code{FALSE} otherwise.
#'
#' @param object an R object
#' @return logical value indicating whether \code{object} is of class
#' \code{iter}
is_iteror <- function(object) {
  inherits(object, "iteror")
}

#' Calls iterators::nextElem(). If error, returns default value.
#'
#' Returns the next element of \code{object}. In the case a StopIteration
#' exception is thrown, the \code{default} value is returned instead.
#'
#' @param object an iterable object
#' @param default default value returned if a StopIteration exception is thrown
#' @param silent Should any errors be suppressed without explicitly notifying
#' the user? Default. Yes
#' @return the next element of \code{object}
try_nextElem <- function(object, default=NA, silent=TRUE) {
  next_elem <- try(iterators::nextElem(object), silent=silent)
  if (stop_iteration(next_elem)) {
    next_elem <- default
  }
  next_elem
}

#' Performs a deep copy of an iterator.
#'
#' This function only works on iterors constructed by
#' [iteror.default](), i.e. iterors created from a constant vector,
#' which have a "state" field. It will not work with name of the
#' higher order iterators created by the "i*()" functions in this
#' package.
#'
#' @export
#' @param iterator an iterator object that inherits from class 'iteror'
#' @return a new iterator with its own state
iter_deepcopy <- function(iterator) {
  iter_copy <- iterator
  iter_copy$state <- new.env(parent=parent.env(iterator$state))

  # Clones iterator's state into iter_copy's state
  state_vars <- ls(envir=iterator$state)
  for (var_i in state_vars) {
    iter_copy$state[[var_i]] <- iterator$state[[var_i]]
  }

  environment(iter_copy$nextOr) <- iter_copy$state

  iter_copy
}
