#' Iterator that extracts the unique elements from an iterable object
#'
#' Constructs an iterator that extracts each unique element in turn from an
#' iterable \code{object}. Order of the elements is maintained. This function is
#' an iterator analogue to [unique].
#'
#' NOTE: In order to determine whether an element is unique, a list of previous
#' unique elements is stored. In doing so, the list can potentially become large
#' if there are a large number of unique elements.
#'
#' @export
#' @param object an iterable object
#' @param digest Optionally specify a custom hash function
#'   (e.g. `digest::digest`, `rlang::hash`). It should be a function
#'   returning a character value.
#' @param ... Extra arguments are forwarded to [iteror].
#' @return an iterator that returns only the unique elements from
#'   \code{object}
#' @seealso idedupe
#'
#' @examples
#' it <- ichain(rep(1, 4), rep(2, 5), 4:7, 2)
#' as.list(iunique(it)) # 1 2 4 5 6 7
#'
#' it2 <- iterators::iter(c('a', 'a', "A", "V"))
#' as.list(iunique(it2)) # a A V
#'
#' x <- as.character(gl(5, 10))
#' it_unique <- iunique(x)
#' as.list(it_unique) # 1 2 3 4 5
#' @importFrom rlang hash
iunique <- function(object, digest=rlang::hash, ...) {
  object <- iteror(object, ...)
  unique_elems <- new.env()
  i <- 1

  nextOr_ <- function(or) {
    repeat {
      i <<- i + 1
      #if (i >= 1000) browser()
      elem <- nextOr(object, return(or))
      h <- digest(elem)
      if (!exists(h, envir=unique_elems)) {
        unique_elems[[h]] <- elem
        return(elem)
      }
    }
  }

  iteror.internal(nextOr_)
}

#' Drop duplicated items from an iterator.
#'
#' Constructs an iterator that removes runs of repeated elements from the
#' underlying iterator. Order of the elements is maintained. Only the element
#' just seen is remembered for determining whether to drop.
#'
#' @export
#' @param object an iterable object
#' @param cmp A function to use for comparison.
#' @param ... passed along to `iteror(object, ...)`
#' @return an iterator that skips over duplicate items from teh
#'   unterlying iterator.
#' @details Originated as `itertools2::iunique_lastseen`.
#'   \code{object}.
#' @seealso irle
#'
#' @examples
#' it <- ichain(rep(1,4), rep(2, 5), 4:7, 2)
#' it_iunique <- idedup(it)
#' as.list(it_iunique) # 1 2 4 5 6 7 2
#'
#' it2 <- iteror(c('a', 'a', "A", 'a', 'a', "V"))
#' idedupe <- idedup(it2)
#' as.list(idedup) # a A a V
#'
idedup <- function(object, cmp=identical, ...) {
  object <- iteror(object, ...)
  prev_elem <- NULL
  first_seen <- FALSE

  nextOr_ <- function(or) {
    repeat {
      elem <- nextOr(object, return(or))
      if (!first_seen || !cmp(elem, prev_elem)) {
        first_seen <<- TRUE
        prev_elem <<- elem
        return(elem)
      }
      prev_elem <<- elem
    }
  }

  iteror.internal(nextOr_)
}

#' Run-length encoding iterator.
#'
#' This is an iterator equivalent of [rle]; it produces one output
#' value for each run if identical values in its input, along with the
#' lenght of the run. `irle_inverse()` performs the inverse
#' transformstion.
#'
#' @author Peter Meilstrup
#' @param obj An iterable
#' @param cmp A function to use for comparison. It should take two
#'   arguments and return `TRUE` or `FALSE`.
#' @param ... further arguments forwarded to [`iteror(obj, ...)`][iteror].
#' @return An iterator returning entries of the form `list(length=n, value=X)`.
#' @author Peter Meilstrup
#' @seealso idedupe
#' @examples
#' it <- isample(c(TRUE, FALSE), 1, replace=TRUE)
#' rle <- irle(it)
#' x <- take(rle, 10)
#' as.logical(irleinv(x))
#' @export irle
irle <- function(obj, cmp=identical, ...) {
  obj <- iteror(obj, ...)
  run <- 0
  ended <- FALSE
  last <- NULL
  #
  nextOr_ <- function(or) {
    if (ended) or
    else repeat {
      val <- obj(or = ended <<- TRUE)
      if (run > 0) {
        if (!ended && cmp(val, last)) {
          run <- run + 1
        } else {
          last_tmp <- last
          run_tmp <- run
          run <<- 1
          last <<- val
          return(list(length=run_tmp, value=last_tmp))
        }
      } else {
        run <<- 1
        last <<- val
      }
    }
  }
  iteror.internal(nextOr_)
}

#' @rdname irle
#' @return `irleinv` recreates the original data from the output of `irle`.
#' @export
irleinv <- function(obj, ...) {
  obj <- iteror(obj, ...)
  count <- 0
  val <- NULL
  nextOr_ <- function(or) {
    while (count <= 0) {
      val <<- obj(or = return(or))
      count <<- val$length
    }
    count <<- count - 1
    val$value
  }
  iteror.internal(nextOr_)
}
