#
# Copyright (c) 2014, Stephen B. Weston
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

imerge <- function(...) {
  heapify <- function(node, max) {
    if (max > 0) {
      repeat {
        lchild <- 2L * node
        rchild <- 2L * node + 1L
        i <- if (lchild <= max && h[[lchild]]$val < h[[node]]$val)
          lchild
        else
          node
        if (rchild <= max && h[[rchild]]$val < h[[i]]$val)
          i <- rchild
        if (i == node)
          break
        tmp <- h[[node]]
        h[[node]] <<- h[[i]]
        h[[i]] <<- tmp
        node <- i
      }
    }
  }

  its <- lapply(list(...), iter)
  h <- vector('list', length(its))
  n <- 0L
  for (it in its) {
    tryCatch({
      h[[n + 1L]] <- list(val=nextElem(it), it=it)
      n <- n + 1L
    },
    error=function(e) {
      # Skip any iterators that don't have any values
      if (!identical(conditionMessage(e), 'StopIteration'))
        stop(e)
    })
  }

  # Convert "h" into a heap
  i <- n %/% 2L
  while (i > 0L) {
    heapify(i, n)
    i <- i - 1L
  }

  nextEl <- function() {
    if (n == 0) {
      stop('StopIteration', call.=FALSE)
    } else {
      val <- h[[1]]$val
      tryCatch({
        h[[1]]$val <<- nextElem(h[[1]]$it)
        heapify(1L, n)
      },
      error=function(e) {
        if (!identical(conditionMessage(e), 'StopIteration'))
          stop(e)
        h[[1]] <<- h[[n]]
        n <<- n - 1L
        heapify(1L, n)
      })
      val
    }
  }

  object <- list(nextElem=nextEl)
  class(object) <- c('abstractiter', 'iter')
  object
}
