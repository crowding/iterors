#
# Copyright (c) 2010, Stephen B. Weston
# Updated 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA

#' Iterators over parallel random-number seeds.
#'
#' The \code{iRNGStream} creates a sequence of random number seeds
#' that are very "far apart" (2^127 steps) in the overall random
#' number sequence, so that each can be used to make a parallel,
#' psudo-independent random iterator. This uses [nextRNGStream] and
#' the "L'Ecuyer-CMRG" generator (for more details on this mechanism,
#' see `vignette("parallel", package="parallel")`.)
#'
#' iRNGSubStream creates seeds that are somewhat less far apart (2^76
#' steps), which can be used as "substream" seeds
#'
#' @aliases iRNGStream iRNGSubStream
#' @param seed Either a single number to be passed to \code{set.seed} or a
#' vector to be passed to \code{nextRNGStream} or \code{nextRNGSubStream}.
#' @seealso \code{\link[base]{set.seed}},
#' \code{\link[parallel]{nextRNGStream}},
#' \code{\link[parallel]{nextRNGSubStream}}
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' global.seed <- .Random.seed
#'
#' rng.seeds <- iRNGStream(313)
#' print(nextOr(rng.seeds))
#' print(nextOr(rng.seeds))
#'
#' # create three pseudo-independent and
#' # reproducible random number generators
#' it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#' it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
#'
#' .Random.seed == global.seed
#' take(it1, 5, "numeric") # 0 0 0 1 1
#' take(it2, 5, "numeric") # 0 1 1 1 1
#' take(it3, 5, "numeric") # 1 1 1 0 0
#'
#' # none of this affects the global seed
#' global.seed == .Random.seed
#'
#' \dontrun{
#' library(foreach)
#' foreach(1:3, rseed=iRNGSubStream(1970), .combine='c') %dopar% {
#'   RNGkind("L'Ecuyer-CMRG") # would be better to initialize workers only once
#'   assign('.Random.seed', rseed, pos=.GlobalEnv)
#'   runif(1)
#' }
#' }
#'
#' @export iRNGStream iRNGSubStream
iRNGStream <- function(seed) {
  # Convert a single number into the appropriate vector for "L'Ecuyer-CMRG"
  if (length(seed) == 1) {
    seed <- convseed(seed)
  }

  # Error checking: this will throw an error right away if the seed is bad
  nextRNGStream(seed)

  # Define the "Next Element" function for the iterator
  nextOr_ <- function(or) (seed <<- nextRNGStream(seed))

  iteror.function(nextOr_)
}

iRNGSubStream <- function(seed) {
  # Convert a single number into the appropriate vector for "L'Ecuyer-CMRG"
  if (length(seed) == 1) {
    seed <- convseed(seed)
  }

  # Error checking: this will throw an error right away if the seed is bad
  nextRNGSubStream(seed)

  # Define the "Next Element" function for the iterator
  nextOr_ <- function() (seed <<- nextRNGSubStream(seed))

  iteror.function(nextOr_)
}

convseed <- function(iseed) {
  saveseed <- if (exists('.Random.seed', where=.GlobalEnv, inherits=FALSE))
    get('.Random.seed', pos=.GlobalEnv, inherits=FALSE)

  saverng <- RNGkind("L'Ecuyer-CMRG")

  tryCatch({
    set.seed(iseed)
    get('.Random.seed', pos=.GlobalEnv, inherits=FALSE)
  },
  finally={
    RNGkind(saverng[1], saverng[2])
    if (is.null(saveseed))
      rm('.Random.seed', pos=.GlobalEnv)
    else
      assign('.Random.seed', saveseed, pos=.GlobalEnv)
  })
}
