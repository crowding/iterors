#
# Copyright (c) 2012, Stephen B. Weston
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



#' Record and replay iterators
#'
#' The \code{irecord} function records the values issued by a specified
#' iterator to a file or connection object.  The \code{ireplay} function
#' returns an iterator that will replay those values.  This is useful for
#' iterating concurrently over multiple, large matrices or data frames that you
#' can't keep in memory at the same time.  These large objects can be recorded
#' to files one at a time, and then be replayed concurrently using minimal
#' memory.
#'
#'
#' @aliases irecord ireplay
#' @param con A file path or open connection.
#' @param iterable The iterable to record to the file.
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' suppressMessages(library(foreach))
#'
#' m1 <- matrix(rnorm(70), 7, 10)
#' f1 <- tempfile()
#' irecord(f1, iteror(m1, by='row', chunksize=3))
#'
#' m2 <- matrix(1:50, 10, 5)
#' f2 <- tempfile()
#' irecord(f2, iteror(m2, by='column', chunksize=3))
#'
#' # Perform a simple out-of-core matrix multiply
#' p <- foreach(col=ireplay(f2), .combine='cbind') %:%
#'        foreach(row=ireplay(f1), .combine='rbind') %do% {
#'          row %*% col
#'        }
#'
#' dimnames(p) <- NULL
#' print(p)
#' all.equal(p, m1 %*% m2)
#' unlink(c(f1, f2))
#'
#' @export irecord ireplay
irecord <- function(con, iterable) {
  if (is.character(con)) {
    con <- file(con, 'wb')
    on.exit(close(con))
  }
  it <- iteror(iterable)
  repeat {
    serialize(nextOr(it, break), con)
  }
  invisible()
}

ireplay <- function(con) {
  # Remember if we had to open this connection
  opened <- if (is.character(con)) {
    con <- file(con, open='rb')
    TRUE
  } else {
    FALSE
  }

  nextOr_ <- function(or) {
    # Check if we've already stopped
    if (is.null(con)) {
      return(or)
    }

    tryCatch({
      unserialize(con)
    },
    error=function(e) {
      if (opened) {
        close(con)
      }
      con <<- NULL
      or
    })
  }

  iteror(nextOr_)
}
