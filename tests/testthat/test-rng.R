`%is%` <- expect_equal

answer <- rbind(c(0, 1, 0, 0, 1),
                c(0, 1, 0, 0, 0),
                c(0, 0, 0, 1, 1))

test_that("convseed", {
  convseed(1)
  convseed(2)
  rng.seeds <- iRNGStream(313)
  nextOr(rng.seeds)
})

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
