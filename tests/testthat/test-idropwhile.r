context("idropwhile iterator")

test_that("Apply idropwhile to an integer sequence", {

  not_too_large <- function(x) {
    x <= 3
  }
  it <- idropwhile(not_too_large, 1:10)

  expect_equal(nextElemOr(it, NA), 4)
  expect_equal(nextElemOr(it, NA), 5)
  expect_equal(nextElemOr(it, NA), 6)
  expect_equal(nextElemOr(it, NA), 7)
  expect_equal(nextElemOr(it, NA), 8)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), 10)
  expect_equal(nextElemOr(it, NA), NA)
})

test_that("Apply idropwhile to an integer sequence using anonymous function", {
  it <- idropwhile(function(x) x <= 10, seq(2, 20, by=2))

  expect_equal(nextElemOr(it, NA), 12)
  expect_equal(nextElemOr(it, NA), 14)
  expect_equal(nextElemOr(it, NA), 16)
  expect_equal(nextElemOr(it, NA), 18)
  expect_equal(nextElemOr(it, NA), 20)
  expect_equal(nextElemOr(it, NA), NA)
})
