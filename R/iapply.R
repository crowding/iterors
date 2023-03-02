#
# Copyright (c) 2008-2010 Revolution Analytics
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



#' Array/Apply Iterator
#'
#' Returns an iterator over an array, which iterates over the array in much the
#' same manner as the \code{apply} function.
#'
#'
#' @param X the array to iterate over.
#' @param MARGIN a vector of subscripts.  \code{1} indicates the first
#' dimension (rows), \code{2} indicates the second dimension (columns), etc.
#' @return The apply iterator.
#' @seealso \code{\link{apply}}
#' @keywords utilities
#' @examples
#'
#' a <- array(1:8, c(2, 2, 2))
#'
#' # iterate over all the matrices
#' it <- iapply(a, 3)
#' as.list(it)
#'
#' # iterate over all the columns of all the matrices
#' it <- iapply(a, c(2, 3))
#' as.list(it)
#'
#' # iterate over all the rows of all the matrices
#' it <- iapply(a, c(1, 3))
#' as.list(it)
#'
#' @export iapply
iapply <- function(X, MARGIN) {
  xit <- icountn(dim(X)[MARGIN])

  nextOr_ <- function(or) {
    i <- nextOr(xit, return(or))
    j <- rep('', length(dim(X)))
    j[MARGIN] <- as.character(i)
    s <- paste('X[', paste(j, collapse=','), ']', sep='')
    x <- parse(text=s)
    eval(x)
  }

  iteror.function(nextOr_)
}
