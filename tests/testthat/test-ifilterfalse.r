context("ifilterfalse iterator")

test_that("Apply ifilterfalse to an integer sequence", {
  is_even <- function(x) {
    x %% 2 == 0
  }
  it <- ifilterfalse(is_even, 1:10)

  expect_equal(nextElemOr(it, NA), 1)
  expect_equal(nextElemOr(it, NA), 3)
  expect_equal(nextElemOr(it, NA), 5)
  expect_equal(nextElemOr(it, NA), 7)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), NA)
})

test_that("Apply ifilterfalse to an integer sequence using anonymous function", {
  it <- ifilterfalse(function(x) x %% 2 == 1, 1:10)

  expect_equal(nextElemOr(it, NA), 2)
  expect_equal(nextElemOr(it, NA), 4)
  expect_equal(nextElemOr(it, NA), 6)
  expect_equal(nextElemOr(it, NA), 8)
  expect_equal(nextElemOr(it, NA), 10)
  expect_equal(nextElemOr(it, NA), NA)
})

test_that("Apply ifilterfalse to a character vector", {
  is_vowel <- function(x) {
    x %in% c('a', 'e', 'i', 'o', 'u')
  }
  it <- ifilterfalse(is_vowel, letters)

  expect_equal(nextElemOr(it, NA), 'b')
  expect_equal(nextElemOr(it, NA), 'c')
  expect_equal(nextElemOr(it, NA), 'd')
  expect_equal(nextElemOr(it, NA), 'f')
  expect_equal(nextElemOr(it, NA), 'g')
  expect_equal(nextElemOr(it, NA), 'h')
  expect_equal(nextElemOr(it, NA), 'j')
  expect_equal(nextElemOr(it, NA), 'k')
  expect_equal(nextElemOr(it, NA), 'l')
  expect_equal(nextElemOr(it, NA), 'm')
  expect_equal(nextElemOr(it, NA), 'n')
  expect_equal(nextElemOr(it, NA), 'p')
  expect_equal(nextElemOr(it, NA), 'q')
  expect_equal(nextElemOr(it, NA), 'r')
  expect_equal(nextElemOr(it, NA), 's')
  expect_equal(nextElemOr(it, NA), 't')
  expect_equal(nextElemOr(it, NA), 'v')
  expect_equal(nextElemOr(it, NA), 'w')
  expect_equal(nextElemOr(it, NA), 'x')
  expect_equal(nextElemOr(it, NA), 'y')
  expect_equal(nextElemOr(it, NA), 'z')
  expect_equal(nextElemOr(it, NA), NA)
})
