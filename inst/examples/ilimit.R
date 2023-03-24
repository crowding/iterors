library(iterors)

ilimit <- function(it, times) {
  it <- iteror(it)

  nextOr_ <- function(or) {
    if (times > 0)
      times <<- times - 1
    else
      or

    nextElem(it)
  }

  iteror.function(nextOr_)
}

it <- ilimit(icount(Inf), 3)
print(nextOr(it, "done"))
print(nextOr(it, "done"))
print(nextOr(it, "done"))
print(nextOr(it, "done"))
