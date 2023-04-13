test_that("Indefinite irecycle of integer sequence", {
  it <- irecycle(1:3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
})

test_that("irecycle repeats integer fixed number of times", {
  it <- irecycle(1:3, times=2)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), NA)
})


test_that("irecycle repeats indefinitely for a function", {
  it <- irecycle(function() rnorm(1))

  set.seed(1)
  i <- nextOr(it, NA)
  set.seed(1)
  expect_equal(i, rnorm(1))

  set.seed(2)
  i <- nextOr(it, NA)
  set.seed(2)
  expect_equal(i, rnorm(1))

  set.seed(3)
  i <- nextOr(it, NA)
  set.seed(3)
  expect_equal(i, rnorm(1))
})

test_that("irecycle repeats a fixed number of times for a function", {
  it <- irecycle(function() rnorm(1), times=3)

  set.seed(1)
  i <- nextOr(it, NA)
  set.seed(1)
  expect_equal(i, rnorm(1))

  set.seed(2)
  i <- nextOr(it, NA)
  set.seed(2)
  expect_equal(i, rnorm(1))

  set.seed(3)
  i <- nextOr(it, NA)
  set.seed(3)
  expect_equal(i, rnorm(1))

  expect_equal(nextOr(it, NA), NA)
})
