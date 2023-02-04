context("itakewhile iterator")

test_that("Apply itakewhile to an integer sequence", {
  not_too_large <- function(x) {
    x <= 5
  }
  it <- itakewhile(not_too_large, 1:100)

  expect_equal(nextElemOr(it, NA), 1)
  expect_equal(nextElemOr(it, NA), 2)
  expect_equal(nextElemOr(it, NA), 3)
  expect_equal(nextElemOr(it, NA), 4)
  expect_equal(nextElemOr(it, NA), 5)
  expect_equal(nextElemOr(it, NA), NA)
})

test_that("Apply itakewhile to an integer sequence using anonymous function", {
  it <- itakewhile(function(x) x <= 10, seq(2, 100, by=2))

  expect_equal(nextElemOr(it, NA), 2)
  expect_equal(nextElemOr(it, NA), 4)
  expect_equal(nextElemOr(it, NA), 6)
  expect_equal(nextElemOr(it, NA), 8)
  expect_equal(nextElemOr(it, NA), 10)
  expect_equal(nextElemOr(it, NA), NA)
})
