context("icompress iterator")

test_that("Apply icompress to an integer sequence", {
  n <- 10
  selectors <- rep(c(F, T), n)
  it <- icompress(1:10, selectors)

  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), 8)
  expect_equal(nextOr(it, NA), 10)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply icompress to an integer sequence using anonymous function", {
  n <- 10
  it <- icompress(1:10, rep(c(T, F), n))

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 7)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply icompress to a character vector", {
  it <- icompress(letters, letters %in% c('a', 'e', 'i', 'o', 'u'))

  expect_equal(nextOr(it, NA), 'a')
  expect_equal(nextOr(it, NA), 'e')
  expect_equal(nextOr(it, NA), 'i')
  expect_equal(nextOr(it, NA), 'o')
  expect_equal(nextOr(it, NA), 'u')
  expect_equal(nextOr(it, NA), NA)
})
