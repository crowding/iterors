context("irepeat iterator")

test_that("Indefinite irepeat of integer", {
  it <- irepeat(42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)
})

test_that("irepeat repeats integer fixed number of times", {
  it <- irepeat(42, times=4)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  i <- nextOr(it, NA)
  expect_equal(i, 42)

  expect_equal(nextOr(it, NA), NA)
})

test_that("irepeat repeats data.frame fixed number of times", {
  it <- irepeat(iris, times=5)

  i <- nextOr(it, NA)
  expect_equal(i, iris)

  i <- nextOr(it, NA)
  expect_equal(i, iris)

  i <- nextOr(it, NA)
  expect_equal(i, iris)

  i <- nextOr(it, NA)
  expect_equal(i, iris)

  i <- nextOr(it, NA)
  expect_equal(i, iris)

  expect_equal(nextOr(it, NA), NA)
})
