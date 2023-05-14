#
# Copyright (c) 2008-2010 Revolution Analytics
# Updated 2023 by Peter Meilstrup
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


#' Iterator Constructor-Constructor Function Wrapper
#'
#' The \code{makeIwrapper} function wraps an R function to produce an
#' iterator constructor.  It is used to construct random sampling
#' iterators in this package; for instance `irnorm` is defined as
#' `irnorm <- makeIwrapper(rnorm)`.
#'
#' The resulting iterator constructors all take an optional
#' `count` argument which specifies the number of times the
#' resulting iterator should fire. They also have an argument
#' `independent` which enables independent tracking of the random
#' number seed. The \code{isample} function is an example of one such
#' iterator constructoe (as are \code{irnorm}, \code{irunif}, etc.).
#'
#' @aliases makeIwrapper
#' @param FUN a function that generates different values each time it
#'   is called; typically one of the standard random number generator
#'   functions.
#' @return An iterator that is a wrapper around the corresponding
#'   function.
#' @details Original version appeared in the `iterators` package.
#' @examples
#'
#' # create an iterator maker for the sample function
#' mysample <- makeIwrapper(sample)
#' # use this iterator maker to generate an iterator that will generate three five
#' # member samples from the sequence 1:100
#' it <- mysample(1:100, 5, count = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # NULL
#' @export
makeIwrapper <- function(FUN) {
  def <- bquote(
    splice=TRUE,
    .(quote(`function`))(
      .(as.pairlist(c(formals(FUN),
                      alist(count=Inf,
                            independent=!missing(seed),
                            seed=)))),
      {
        list(..(lapply(names(formals(FUN)), as.name)))
        if (independent) {
          ## if (RNGkind()[1] != "L'Ecuyer-CMRG") {
          ## message("Independent random number stream requested; changing to `RNGkind(\"L'Ecuyer-CMRG\")`. (To suppress this message, run that command yourself.)")
          ##   RNGkind("L'Ecuyer-CMRG")
          ## }
          if (!exists(".Random.seed", where=.GlobalEnv, inherits=FALSE)) {
            RNGkind("L'Ecuyer-CMRG")
            set.seed(NULL)
          }

          if (missing(seed))
            seed <- rng.state$stream() # see iterors-package.R
          if (length(seed) == 1)
            seed <- convseed(seed)
          # Error checking: this will throw an error right away if the
          # seed is bad
          nextRNGStream(seed)

          next_ <- function(or) {
            if (count > 0) {
              count <<- count - 1L
              oldSeed <- .Random.seed
              oldKind <- RNGkind("L'Ecuyer-CMRG")[1]
              assign('.Random.seed', seed, envir=.GlobalEnv)
              on.exit({
                RNGkind(oldKind)
                assign('.Random.seed', oldSeed, envir=.GlobalEnv)
              })
              val <- .(substitute(FUN))(
                ..(structure(lapply(names(formals(FUN)), as.name),
                             names=names(formals(FUN)))))
              seed <<- .Random.seed
              val
            } else {
              return(or)
            }
          }
          iteror(next_)
        } else {
          next_ <- function(or) {
            if (count > 0) {
              count <<- count - 1L
              .(substitute(FUN))(
                ..(structure(lapply(names(formals(FUN)), as.name),
                             names=names(formals(FUN)))))
            } else {
              return(or)
            }
          }
          iteror(next_)
        }
      })
    )
  eval(def, parent.frame())
}


#' @rdname rng
#' @title Random Number Iterators
#'
#' @description These function returns an iterator that returns random
#'   numbers of various distributions.  Each one is a wrapper around a
#'   standard \code{R} function.
#'
#' @param count number of times that the iterator will fire.  If not
#'   specified, it will fire values forever.
#' @param independent If TRUE, this iterator will keep its own private
#'   random state, so that its output is reproducible and independent
#'   of anything else in the program; this comes at some performance
#'   cost. If you do not specify `seed` a seed value will be chosen
#'   for you.
#' @param seed A specific seed value for reproducibility. If given,
#'   `independent=TRUE` is implied. Well separated seed values can be
#'   obtained from [iRNGStream].
#'
#' @return An iterator that is a wrapper around the corresponding
#'   random number generator function.
#' @param n How many samples to compute per call; see e.g. [rnorm].
#' @param mean see [rnorm].
#' @param sd see [rnorm].
#' @param size see e.g. [rbinom].
#' @param prob see e.g. [rbinom].
#' @param mu see [rnbinom].
#' @param lambda see [rpois].
#' @param x see [isample].
#' @param replace see [isample].
#' @param min see [runif].
#' @param max see [runif].
#'
#' @details Originally from the `iterators` package.
#' @examples
#'
#' # create an iterator that returns three random numbers
#' it <- irnorm(1, count = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)
#'
#' # iterators created with a specific seed will produce reproducible values
#' it <- irunif(n=1, seed=314)
#' nextOr(it) # 0.4936700
#' nextOr(it) # 0.5103891
#' nextOr(it) # 0.2338745
#' @importFrom stats rbinom rnbinom rnorm rpois runif
#' @export irnorm irbinom irnbinom irpois isample irunif
#' @aliases irnorm irunif irbinom irnbinom irpois isample

# function() NULL is to quiet CMD check "no visible global function definition"
irnorm <- function() NULL
irnorm <- makeIwrapper(rnorm)
#' @rdname rng
irbinom <- function() NULL
irbinom <- makeIwrapper(rbinom)
#' @rdname rng
irnbinom <- function() NULL
irnbinom <- makeIwrapper(rnbinom)
#' @rdname rng
irpois <- function() NULL
irpois <- makeIwrapper(rpois)
#' @rdname rng
isample <- function() NULL
isample <- makeIwrapper(sample)
#' @rdname rng
irunif <- function() NULL
irunif <- makeIwrapper(runif)

