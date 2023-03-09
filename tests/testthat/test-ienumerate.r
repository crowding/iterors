context("ienumerate iterator")

test_that("ienumerate over numeric vector", {
  set.seed(42)
  x <- rnorm(5)

  it <- ienumerate(x)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, x[1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, x[2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, x[3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, x[4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, x[5])

  expect_equal(nextOr(it, NA), NA)
})

test_that("ienum over numeric vector", {
  set.seed(42)
  x <- rnorm(5)

  it <- ienum(x)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, x[1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, x[2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, x[3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, x[4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, x[5])

  expect_equal(nextOr(it, NA), NA)
})

test_that("ienumerate over data.frame", {
  it <- ienumerate(iris)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, iris[, 1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, iris[, 2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, iris[, 3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, iris[, 4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, iris[, 5])

  expect_equal(nextOr(it, NA), NA)
})

test_that("ienum over data.frame", {
  it <- ienum(iris)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, iris[, 1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, iris[, 2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, iris[, 3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, iris[, 4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, iris[, 5])

  expect_equal(nextOr(it, NA), NA)
})
