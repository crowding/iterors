#
# Copyright (c) 2009-2010, Stephen B. Weston
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



#' Create a cartesian product iterator
#'
#' Create an iterator that returns values from multiple iterators in cartesian
#' product fashion.  That is, they are combined the manner of nested \code{for}
#' loops.
#'
#'
#' @param \dots Named iterables to iterate over.  The right-most iterables
#' change more quickly, like an odometer.
#' @keywords utilities
#' @examples
#'
#' # Simulate a doubly-nested loop with a single while loop
#' it <- ihasNext(product(a=1:3, b=1:2))
#' while (hasNext(it)) {
#'   x <- nextElem(it)
#'   cat(sprintf('a = %d, b = %d\n', x$a, x$b))
#' }
#'
#' @export product
product <- function(...) {
  args <- substitute(list(...))[-1]
  n <- length(args)
  anames <- names(args)
  if (is.null(anames)) {
    anames <- rep('', n)
  }
  env <- parent.frame()
  product.internal(n, args, anames, env)
}

product.internal <- function(n, args, anames, env) {
  if (n <= 1) {
    if (n == 1) {
      icar <- iter(eval(args[[1]], envir=env))

      nextEl <- function() {
        carval <- list(nextElem(icar))
        names(carval) <- anames[1]
        carval
      }
    } else {
      nextEl <- function() {
        stop('StopIteration', call.=FALSE)
      }
    }
  } else {
    icdr <- product.internal(n - 1, args[-n], anames[-n], env)
    cdrval <- NULL
    needval <- TRUE
    icar <- NULL

    nextEl <- function() {
      repeat {
        if (needval) {
          cdrval <<- nextElem(icdr)
          needval <<- FALSE
          icar <<- iter(eval(args[[n]], envir=env))
        }

        tryCatch({
          carval <- list(nextElem(icar))
          break
        },
        error=function(e) {
          if (identical(conditionMessage(e), 'StopIteration')) {
            needval <<- TRUE
          } else {
            stop(e)
          }
        })
      }

      names(carval) <- anames[n]
      c(cdrval, carval)
    }
  }

  object <- list(nextElem=nextEl)
  class(object) <- c('abstractiter', 'iter')
  object
}
