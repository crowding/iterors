#' Construct a sliding window over an iterator
#'
#' Each element returned by `iwindow(obj)` consists of `n` consecutive
#' elements from the underlying `obj`, with the window advancing
#' forward by one element each iteration.
#' @param obj An iterable.
#' @param n The width of the window to apply
#' @param tail If a value is given, tails will be included at
#' the beginning and end of iteration, filled with the given value.
#' @return an iteror.
#' @author Peter Meilstrup
#' @export
#' @examples
#'
#' #' @examples
#' it <- iwindow(iteror(letters[1:4]), 2)
#' nextOr(it, NA) # list("a", "b")
#' nextOr(it, NA) # list("b", "c")
#' nextOr(it, NA) # list("c", "d")
#'
#' it2 <- iwindow(icount(5), 2)
#' nextOr(it2, NA) # list(1, 2)
#' nextOr(it2, NA) # list(2, 3)
#' nextOr(it2, NA) # list(3, 4)
#' nextOr(it2, NA) # list(4, 5)
#'
#' it <- iwindow(letters[1:4], 2)
#' nextOr(it, NA) # list("a", "b")
#' nextOr(it, NA) # list("b", "c")
#' nextOr(it, NA) # list("c", "d")
#'
#' it <- iwindow(letters[1:4], 3)
#' nextOr(it) # list("a", "b", "c")
#' nextOr(it) # list("b", "c", "d")
#'
#' it <- iwindow(letters[1:4], 3, tail=" ")
#' nextOr(it) # list(" ", " ", "a")
#' nextOr(it) # list(" ", "a", "b")
#' nextOr(it) # list("a", "b", "c")
#' nextOr(it) # list("b", "c", "d")
#' nextOr(it) # list("c", "d", " ")
#' nextOr(it) # list("d", " ", " ")
iwindow <- function(obj, n, tail) {
  if (n==2 && missing(tail)) {
    return(ipairwise(obj))
  } else if (n==3 && missing(tail)) {
    return(itripletwise(obj))
  }
  hasTail <- !missing(tail)
  list(obj, n)
  dq <- deque()
  obj <- iteror(obj)

  stage <- "start"
  nn <- 0

  nextOr_ <- function(or) {
    repeat switch(stage,
      start={
        if (nn < n-1) {
          if (hasTail)
            dq$append(tail)
          else dq$append(obj(or={
            stage <<- "end"; next
          }))
          nn <<- nn + 1
        } else {
          stage <<- "middle"
        }
      },
      middle={
        dq$append(obj(or={
          stage <<- "end"; next
        }))
        val <- dq$extract()
        dq$getFirst()
        return(val)
      },
      end={
        if (nn > 0) {
          if (hasTail) {
            dq$append(tail)
            val <- dq$extract(seq_len(n))
            dq$getFirst()
            nn <<- nn - 1
            return(val)
          } else {
            dq$getFirst()
            nn <<- nn - 1
          }
        } else return(or)
      })
  }

  iteror.internal(nextOr_)
}


itripletwise <- function(obj) {
  obj <- iteror(obj)
  init <- FALSE
  last_1 <- NULL
  last_2 <- NULL

  nextOr_ <- function(or) {
    if (!init) {
      last_2 <<- obj(or=return(or))
      last_1 <<- obj(or=return(or))
      init <<- TRUE
    }
    list(last_2,
         last_2 <<- last_1,
         last_1 <<- obj(or=return(or)))
  }

  iteror(nextOr_)
}


ipairwise <- function(obj) {
  obj <- iteror(obj)
  init <- FALSE
  last <- NULL

  nextOr_ <- function(or) {
    if (!init) {
      last <<- obj(or=return(or))
      init <<- TRUE
    }
    list(last, last <<- obj(or=return(or)))
  }

  iteror(nextOr_)
}
