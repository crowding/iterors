`%is%` <- expect_equal

answer <- rbind(c(0, 1, 0, 0, 1),
                c(0, 1, 0, 0, 0),
                c(0, 0, 0, 1, 1))

x <- rnorm(1)

test_that("RNG streams are reproducible", {

  global.seed <- .Random.seed
  rng.seeds <- iRNGStream(313)
  # create three pseudo-independent and
  # reproducible random number generators
  it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds), independent=TRUE)
  it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds), independent=TRUE)
  it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds), independent=TRUE)

  expect_true(identical(.Random.seed, global.seed))
  take(it1, 5, "numeric") %is% answer[1,]
  take(it2, 5, "numeric") %is% answer[2,]
  take(it3, 5, "numeric") %is% answer[3,]

  # none of this affects the global seed
  expect_true(identical(.Random.seed, global.seed))
})

test_that("RNG streams are independent", {

  rng.seeds <- iRNGStream(313)
  it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds), independent=TRUE)
  it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds), independent=TRUE)
  it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds), independent=TRUE)

  # and this is independent of order in which
  # we call them...
  result <- take(iroundrobin(it1, it2, it3), 15, "numeric")
  expect_true(identical(matrix(result, 3, 5), answer))

})

test_that("converting seeds", {
  if (exists('.Random.seed', where=.GlobalEnv, inherits=FALSE))
    rm('.Random.seed', pos=.GlobalEnv)

  st <- iRNGStream(42069)
  expect_equal(nextOr(st),
               c(10407L, 1962334501L,
                 1957360453L, 182261958L,
                 1552366930L,  -1175365792L,
                 -1765451585L))

  expect_false(exists(".Random.seed", envir=.GlobalEnv))
})

test_that("substream", {

  sst <- iRNGSubStream(42069)
  it <- isample(c(0, 1), 100,
                replace=TRUE, independent=TRUE, seed=sst())

  nextOr(it) %is% c(1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1,
                    0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,
                    0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0,
                    0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1,
                    1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0)
  rnorm(1)
  nextOr(it) %is% c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                    1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
                    0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
                    1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
                    0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0,
                    1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0)

})
