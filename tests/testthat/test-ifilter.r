context("ifilter iterator")

test_that("Apply ifilter to an integer sequence", {
  is_even <- function(x) {
    x %% 2 == 0
  }
  it <- ifilter(is_even, 1:10)

  expect_equal(nextElemOr(it, NA), 2)
  expect_equal(nextElemOr(it, NA), 4)
  expect_equal(nextElemOr(it, NA), 6)
  expect_equal(nextElemOr(it, NA), 8)
  expect_equal(nextElemOr(it, NA), 10)
  expect_equal(nextElemOr(it, NA), NA)
})

test_that("Apply ifilter to an integer sequence using anonymous function", {
  it <- ifilter(function(x) x %% 2 == 1, 1:10)

  expect_equal(nextElemOr(it, NA), 1)
  expect_equal(nextElemOr(it, NA), 3)
  expect_equal(nextElemOr(it, NA), 5)
  expect_equal(nextElemOr(it, NA), 7)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), NA)
})

test_that("Apply ifilter to a character vector", {
  is_vowel <- function(x) {
    x %in% c('a', 'e', 'i', 'o', 'u')
  }
  it <- ifilter(is_vowel, letters)

  expect_equal(nextElemOr(it, NA), 'a')
  expect_equal(nextElemOr(it, NA), 'e')
  expect_equal(nextElemOr(it, NA), 'i')
  expect_equal(nextElemOr(it, NA), 'o')
  expect_equal(nextElemOr(it, NA), 'u')
  expect_equal(nextElemOr(it, NA), NA)
})
