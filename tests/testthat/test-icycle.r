context("icycle iterator")

test_that("Indefinite icycle of integer sequence", {
  it <- icycle(1:3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
})

test_that("icycle repeats integer fixed number of times", {
  it <- icycle(1:3, times=2)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), NA)
})


test_that("icycle repeats indefinitely for a function", {
  it <- icycle(function() rnorm(1))

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

test_that("icycle repeats a fixed number of times for a function", {
  it <- icycle(function() rnorm(1), times=3)

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
