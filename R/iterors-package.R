rng.state <- new.env()

.onLoad <- function(lib, pkg) {
  rng.state$stream <- iRNGStream(.Random.seed)
}
