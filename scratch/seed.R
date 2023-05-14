RNGkind("L'Ecuyer-CMRG")
set.seed(4039)
seeds <- list()
seeds$a <- nextRNGStream(.Random.seed)
seeds$b <- nextRNGStream(seeds$a)

withSeed <- function(whichStream, rng, ...) {
  old.seed <- .Random.seed
  set.seed(seeds[[whichStream]])
  #assign(".Random.seed", .GlobalEnv)
  seeds$a <<- rng(...)
  set.seed(oldSeed)
}

# I am confused because help("rng") says that the `seed` argument of `set.seed` takes a single integer, whereas 


RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
saved <- .Random.seed
generated1 <- rnorm(100)
set.seed(saved)
identical(.Random.seed, saved)
