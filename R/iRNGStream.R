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

#' Iterators that support parallel RNG
#'
#' The \code{iRNGStream} function creates an infinite iterator that calls
#' \code{nextRNGStream} repeatedly, and \code{iRNGSubStream} creates an
#' infinite iterator that calls \code{nextRNGSubStream} repeatedly.
#'
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
#' it <- iRNGStream(313)
#' print(nextOr(it))
#' print(nextOr(it))
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
#' @export iRNGStream
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
