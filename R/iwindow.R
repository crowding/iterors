#' Construct a sliding window over an iterator
#'
#' Each element returned by `iwindow(obj)` consists of `n` consecutive
#' elements from the underlying `obj`, with the window advancing
#' forward by one element each iteration.
#' @param obj An iterable.
#' @param n The width of the window to apply
#' @param tails Whether to include tails at the beginning and end. The
#'   tails will be filled with NULL.
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
#' it <- iwindow(letters[1:4], 3, tails=TRUE)
#' nextOr(it) # list(NULL, NULL, "a")
#' nextOr(it) # list(NULL, "a", "b")
#' nextOr(it) # list("a", "b", "c")
#' nextOr(it) # list("b", "c", "d")
#' nextOr(it) # list("c", "d", NULL)
#' nextOr(it) # list("d", NULL, NULL)
iwindow <- function(obj, n, tails=FALSE) {
  if (n==2 && tails==FALSE) {
    return(ipairwise(obj))
  } else if (n==3 && tails==FALSE) {
    return(itripletwise(obj))
  }
  list(obj, n, tails)
  dq <- deque()
  obj <- iteror(obj)

  stage <- "start"
  nn <- 0

  nextOr_ <- function(or) {
    repeat switch(stage,
      start={
        if (nn < n-1) {
          if (tails)
            dq$append(NULL)
          else dq$append(nextOr(obj, or={
            stage <<- "end"; next
          }))
          nn <<- nn + 1
        } else {
          stage <<- "middle"
        }
      },
      middle={
        dq$append(nextOr(obj, or={
          stage <<- "end"; next
        }))
        val <- dq$extract()
        dq$getFirst()
        return(val)
      },
      end={
        if (nn > 0) {
          if (tails) {
            val <- dq$extract(seq_len(n), NA)
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

  iteror.function(nextOr_)
}


itripletwise <- function(obj) {
  obj <- iteror(obj)
  init <- FALSE

  nextOr_ <- function(or) {
    if (!init) {
      last_2 <<- nextOr(obj, return(or))
      last_1 <<- nextOr(obj, return(or))
      init <<- TRUE
    }
    list(last_2,
         last_2 <<- last_1,
         last_1 <<- nextOr(obj, return(or)))
  }

  iteror(nextOr_)
}


ipairwise <- function(obj) {
  obj <- iteror(obj)
  init <- FALSE

  nextOr_ <- function(or) {
    if (!init) {
      last <<- nextOr(obj, return(or))
      init <<- TRUE
    }
    list(last, last <<- nextOr(obj, return(or)))
  }

  iteror(nextOr_)
}
