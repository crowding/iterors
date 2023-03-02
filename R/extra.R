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

# This function makes iterator makers.  The resulting iterator makers all take
# an optional "count" argument which specifies the number of times the
# resulting iterator should fire.  The iterators are wrappers around functions
# that return different values each time they are called.  All this is done to
# avoid cutting and pasting the same code repeatedly.


#' Iterator Maker Generator
#'
#' The \code{makeIwrapper} function makes iterator makers.  The resulting
#' iterator makers all take an optional \code{count} argument which specifies
#' the number of times the resulting iterator should fire.  The iterators are
#' wrappers around functions that return different values each time they are
#' called. The \code{isample} function is an example of one such iterator maker
#' (as are \code{irnorm}, \code{irunif}, etc.).
#'
#'
#' @aliases makeIwrapper isample
#' @param FUN a character string naming a function that generates different
#' values each time it is called; typically one of the standard random number
#' generator functions.
#' @param count number of times that the iterator will fire.  If not specified,
#' it will fire values forever.
#' @param \dots arguments to pass to the underlying \code{FUN} function.
#' @return An iterator that is a wrapper around the corresponding function.
#' @keywords utilities
#' @examples
#'
#' # create an iterator maker for the sample function
#' mysample <- makeIwrapper("sample")
#' # use this iterator maker to generate an iterator that will generate three five
#' # member samples from the sequence 1:100
#' it <- mysample(1:100, 5, count = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # NULL
#'
#' @export makeIwrapper
makeIwrapper <- function(FUN) {
  function(..., count) {
    if (!missing(count) && (!is.numeric(count) || length(count) != 1))
      stop('count must be a numeric value')

    # construct the call object to put into the nextOr function
    m <- as.call(c(as.name(FUN), list(...)))

    # construct the body of the nextOr function
    fbody <- if (missing(count)) {
      m
    } else {
      substitute({
        if (count > 0) {
          count <<- count - 1L
          REPLACETHIS
        } else {
          return(or)
        }
      }, list(REPLACETHIS=m))
    }

    # create the nextOr function using fbody
    nextOr_ <- function(or) NULL
    body(nextOr_) <- fbody

    # create and return the iterator object
    iteror.function(nextOr_)
  }
}

#' Random Number Iterators
#'
#' These function returns an iterators that return random numbers of various
#' distributions.  Each one is a wrapper around a standard \code{R} function.
#'
#'
#' @aliases irnorm irunif irbinom irnbinom irpois
#' @param count number of times that the iterator will fire.  If not specified,
#' it will fire values forever.
#' @param \dots arguments to pass to the underlying \code{rnorm} function.
#' @return An iterator that is a wrapper around the corresponding random number
#' generator function.
#' @keywords utilities
#' @examples
#'
#' # create an iterator that returns three random numbers
#' it <- irnorm(1, count = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect a StopIteration exception
#'
#' @export irnorm irbinom irnbinom irpois isample irunif
irnorm <- makeIwrapper('rnorm')
irbinom <- makeIwrapper('rbinom')
irnbinom <- makeIwrapper('rnbinom')
irpois <- makeIwrapper('rpois')
isample <- makeIwrapper('sample')
irunif <- makeIwrapper('runif')


#' Counting Iterators
#'
#' Returns an iterator that counts starting from one.
#'
#'
#' @aliases icount icountn
#' @param count number of times that the iterator will fire.  If not specified,
#' it will count forever.
#' @param vn vector of counts.
#' @return The counting iterator.
#' @keywords utilities
#' @examples
#'
#' # create an iterator that counts from 1 to 3.
#' it <- icount(3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' @export icount icountn
icount <- function(count) {
  if (missing(count))
    count <- NULL
  else if (!is.numeric(count) || length(count) != 1)
    stop('count must be a numeric value')

  i <- 0L

  if (is.null(count))
    nextOr_ <- function(or) {
      (i <<- i + 1L)
    }
  else
    nextOr_ <- function(or) {
      if (i < count)
        (i <<- i + 1L)
      else
        or
    }

  iteror.function(nextOr_)
}

#' Dividing Iterator
#'
#' Returns an iterator that returns pieces of numeric value.
#'
#'
#' @param n number of times that the iterator will fire.  If not specified, it
#' will count forever.
#' @param \dots unused.
#' @param chunks the number of pieces that \code{n} should be divided into.
#' This is useful when you know the number of pieces that you want.  If
#' specified, then \code{chunkSize} should not be.
#' @param chunkSize the maximum size of the pieces that \code{n} should be
#' divided into.  This is useful when you know the size of the pieces that you
#' want.  If specified, then \code{chunks} should not be.
#' @return The dividing iterator.
#' @keywords utilities
#' @examples
#'
#' # divide the value 10 into 3 pieces
#' it <- idiv(10, chunks = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # expect NULL
#'
#' # divide the value 10 into pieces no larger than 3
#' it <- idiv(10, chunkSize = 3)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#' nextOr(it, NULL)  # end of iterator
#'
#' @export idiv
idiv <- function(n, ..., chunks, chunkSize) {
  if (!is.numeric(n) || length(n) != 1)
    stop('n must be a numeric value')

  if (length(list(...)) > 0)
    stop('chunks and chunkSize must be specified as named arguments')

  if ((missing(chunkSize) && missing(chunks)) ||
      (!missing(chunkSize) && !missing(chunks)))
    stop('either chunks or chunkSize must be specified, but not both')

  if (missing(chunks)) {
    if (!is.numeric(chunkSize) || length(chunkSize) != 1 || chunkSize < 1)
      stop('chunkSize must be a numeric value >= 1')
    chunks <- ceiling(n / chunkSize)
  }

  nextOr_ <- function(or) {
    if (chunks <= 0 || n <= 0)
      return(or)

    m <- ceiling(n / chunks)
    n <<- n - m
    chunks <<- chunks - 1
    m
  }

  iteror.function(nextOr_)
}


#' Iterator over Lines of Text from a Connection
#'
#' Returns an iterator over the lines of text from a connection.  It is a
#' wrapper around the standard \code{readLines} function.
#'
#'
#' @param con a connection object or a character string.
#' @param n integer.  The maximum number of lines to read.  Negative values
#' indicate that one should read up to the end of the connection.  The default
#' value is 1.
#' @param \dots passed on to the \code{readLines} function.
#' @return The line reading iterator.
#' @seealso \code{\link[base]{readLines}}
#' @keywords utilities
#' @examples
#'
#' # create an iterator over the lines of COPYING
#' it <- ireadLines(file.path(R.home(), "COPYING"))
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#'
#' @export ireadLines
ireadLines <- function(con, n=1, ...) {
  if (!is.numeric(n) || length(n) != 1 || n < 1)
    stop('n must be a numeric value >= 1')

  if (is.character(con)) {
    con <- file(con, open='r')
    doClose <- TRUE
  } else {
    doClose <- FALSE
  }

  nextOr_ <- function(or) {
    if (is.null(con))
      return(or)

    r <- readLines(con, n=n, ...)
    if (length(r) == 0) {
      if (doClose)
        close(con)
      con <<- NULL
      return(or)
    }
    r
  }

  iteror.function(nextOr_)
}


#' Iterator over Rows of a Data Frame Stored in a File
#'
#' Returns an iterator over the rows of a data frame stored in a file in table
#' format.  It is a wrapper around the standard \code{read.table} function.
#'
#'
#' @param file the name of the file to read the data from.
#' @param \dots all additional arguments are passed on to the \code{read.table}
#' function.  See the documentation for \code{read.table} for more information.
#' @param verbose logical value indicating whether or not to print the calls to
#' \code{read.table}.
#' @return The file reading iterator.
#' @note In this version of \code{iread.table}, both the \code{read.table}
#' arguments \code{header} and \code{row.names} must be specified.  This is
#' because the default values of these arguments depend on the contents of the
#' beginning of the file.  In order to make the subsequent calls to
#' \code{read.table} work consistently, the user must specify those arguments
#' explicitly.  A future version of \code{iread.table} may remove this
#' requirement.
#' @seealso \code{\link[utils]{read.table}}
#' @keywords utilities
#' @export iread.table
iread.table <- function(file, ..., verbose=FALSE) {
  args <- list(...)
  argnames <- names(args)

  # need to do this (at least for now) because the default values for
  # header and row.names depend on the first few lines of the file,
  # which could cause a different number of columns to be returned from
  # the first versus the subsequent calls to read.table
  if (!all(c('header', 'row.names') %in% argnames))
    stop('both header and row.names must be specified in this implementation')

  nrows <- if ('nrows' %in% argnames) args$nrows else 1
  row.names <- args$row.names

  # it doesn't seem to make sense to allow nrows < 1 for the "iterator"
  # version of read.table
  if (!is.numeric(nrows) || length(nrows) != 1 || nrows < 1)
    stop('nrows must be a numeric value >= 1')

  # open the file if necessary and remember to close it
  if (is.character(file)) {
    file <- file(file, open='r')
    doClose <- TRUE
  } else {
    doClose <- FALSE
  }

  # create the call object that we'll use to call read.table
  m <- as.call(c(as.name('read.table'), file='', list(...)))
  m$file <- file
  m$nrows <- nrows  # needed since we use a different default than read.table
  env <- sys.frame(sys.nframe())

  # compute these once rather than repeatedly
  rnlen <- length(row.names)
  gotrownames <- is.character(row.names) && rnlen > 1

  # initialize a few state variables
  first.time <- TRUE
  irow <- 1
  errmsg <- NULL

  nextOr_ <- function(or) {
    if (!is.null(errmsg))
      stop(paste('iterator failed previously:', errmsg), call.=FALSE)

    if (is.null(file))
      return(or)

    if (gotrownames) {
      rem <- rnlen - irow + 1  # remaining strings in row.names
      nrows <<- min(nrows, rem)  # possibly decrease nrows to match row.names

      # there is a problem if nrows is one: we would have to set row.names
      # to a character vector of length one, which is interpreted
      # incorrectly by read.table
      if (nrows > 1)
        m$row.names <<- row.names[seq(irow, length=nrows)]
      else
        m['row.names'] <<- list(NULL)  # we'll fix the row names later
      m$nrows <<- nrows
    }

    # call read.table to actually read the file
    r <- tryCatch({
      # handle the case where we've run out of row names
      if (nrows > 0) {
        if (verbose)
          print(m)
        eval(m, env)
      } else {
        NULL
      }
    },
    error=function(e) {
      # this error is thrown at the end of input sometimes
      # but other times a data frame with no rows is returned
      # (for instance when col.names is specified)
      if (!identical(conditionMessage(e), 'no lines available in input')) {
        if (doClose)
          close(file)
        file <<- NULL
        errmsg <<- conditionMessage(e)
        stop(e)
      }
      NULL
    })

    # set header to FALSE, skip to 0, and col.names to names(r)
    # after the first call to read.table
    if (first.time) {
      first.time <<- FALSE
      m$header <<- FALSE
      m$skip <<- 0
      nms <- names(r)
      if (is.numeric(row.names)) {
        nms <- if (row.names == 1)
          c('', nms)
        else if (row.names >= length(nms))
          c(nms, '')
        else
          c(nms[1:(row.names-1)], '', nms[row.names:length(nms)])
      }
      m$col.names <<- nms
    }

    # check if we're done reading
    if (is.null(r) || nrow(r) == 0) {
      if (doClose)
        close(file)
      file <<- NULL
      return(or)
    }

    if (gotrownames) {
      # fix the row names for this particular case
      if (nrows == 1)
        rownames(r) <- row.names[irow]

      # update the index into row.names
      irow <<- irow + nrows
    }

    r
  }

  iteror.function(nextOr_)
}
