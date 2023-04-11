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



#' Apply a function to each element of an iterator.
#'
#' `iapply(obj, f)` returns the iteror that applies `f` to
#' each element of the given iterable `obj`. It is an iterator
#' equivalent of `lapply`.
#'
#' @param obj an iterable.
#' @param f a function
#' @param ... Other arguments that will be passed along to `f`
#' @return An iteror.
#' @seealso \code{\link{imap}} To applying a function to parallel iterables in parallel. [iteror.array]
#'
#' The `iterators` package included an `iapply` to iterate over the
#' margins of an array; for equivalent behavior you can use
#' `iteror(ARRAY, by=MARGIN)]`
#' @keywords utilities
#' @export iapply
iapply <- function(obj, f, ...) {
  obj <- iteror(obj, ...)
  nextOr_ <- function(or) f(nextOr(obj, return(or)), ...)
  iteror.internal(nextOr_)
}
