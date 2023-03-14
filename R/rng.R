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


# Iterator Maker Generator (internal function, used at package init)
#
# The \code{makeIwrapper} function makes iterator makers.  The resulting
# iterator makers all take an optional \code{count} argument which specifies
# the number of times the resulting iterator should fire.  The iterators are
# wrappers around functions that return different values each time they are
# called. The \code{isample} function is an example of one such iterator maker
# (as are \code{irnorm}, \code{irunif}, etc.).
#
# @aliases makeIwrapper
# @param FUN a character string naming a function that generates different
# values each time it is called; typically one of the standard random number
# generator functions.
# @param count number of times that the iterator will fire.  If not specified,
# it will fire values forever.
# @param \dots arguments to pass to the underlying \code{FUN} function.
# @return An iterator that is a wrapper around the corresponding function.
# @keywords utilities
# @details Originally from the `iterators` package.
# @examples
#
# # create an iterator maker for the sample function
# mysample <- makeIwrapper("sample")
# # use this iterator maker to generate an iterator that will generate three five
# # member samples from the sequence 1:100
# it <- mysample(1:100, 5, count = 3)
# nextOr(it)
# nextOr(it)
# nextOr(it)
# nextOr(it, NULL)  # NULL

makeIwrapper <- function(FUN) {

  def <- bquote(
    splice=TRUE,
    .(quote(`function`))(
      .(as.pairlist(c(formals(FUN),
                      alist(count=Inf,
                            independent=TRUE,
                            seed=nextOr(rng.state$stream))))),
      {
        list(..(lapply(names(formals(FUN)), as.name)))
        if (independent) {
          force(seed)
          # Error checking: this will throw an error right away if the seed is bad
          nextRNGStream(seed)

          next_ <- function(or) {
            if (count > 0) {
              count <<- count - 1L

              oldSeed <- .Random.seed
              oldKind <- RNGkind("L'Ecuyer-CMRG")[1]
              assign('.Random.seed', seed, pos=.GlobalEnv)
              on.exit({
                RNGkind(oldKind)
                assign('.Random.seed', oldSeed, pos=.GlobalEnv)
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
          iteror.function(next_)
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
          iteror.function(next_)
        }
      }))

  eval(def, parent.frame())
}

#' Random Number Iterators
#'
#' These function returns an iterators that return random numbers of various
#' distributions.  Each one is a wrapper around a standard \code{R} function.
#'
#'
#' @aliases irnorm irunif irbinom irnbinom irpois
#' @param count number of times that the iterator will fire.  If not
#'   specified, it will fire values forever.
#' @param \dots arguments to pass to the underlying \code{rnorm}
#'   function.
#' @param independent default TRUE means that each iterator will keep
#'   its own independent RNG seed; meaning that rendom number
#'   iterators will be truly independent of any others. If FALSE,
#'   rendom number generation
#' @param seed A seed value usable by the "L'Ecuyer-CMRG"
#'   generator. The default will create a pseudo-independent stream
#'   for each newly constructed iterator. You can specify a specific
#'   value for reproducibility. To reproduciby greate several
#'   independent random numer iterators see the example under
#'   [iRNGStream].
#' @return An iterator that is a wrapper around the corresponding
#'   random number generator function.
#' @keywords utilities
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
#' # iterators with the same seed will produce the same values
#' it <- irunif(it, seed=0.4812097)
#'
#' @export irnorm irbinom irnbinom irpois isample irunif
irnorm <- makeIwrapper(rnorm)
irbinom <- makeIwrapper(rbinom)
irnbinom <- makeIwrapper(rnbinom)
irpois <- makeIwrapper(rpois)
isample <- makeIwrapper(sample)
irunif <- makeIwrapper(runif)

