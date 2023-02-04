context("imap iterator")

pow <- function(x, y) {
  x^y
}

test_that("imap over two numeric vectors of equal length", {
  it <- imap(pow, c(2, 3, 10), c(5, 2, 3))

  expect_equal(nextElemOr(it, NA), 32)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), 1000)

  expect_equal(nextElemOr(it, NA), NA)
})

test_that("imap over two numeric vectors of unequal length", {
  it <- imap(pow, c(2, 3, 10), c(5, 2, 3, 42))

  expect_equal(nextElemOr(it, NA), 32)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), 1000)

  expect_equal(nextElemOr(it, NA), NA)
})

test_that("imap over two lists of equal length", {
  it <- imap(pow, list(2, 3, 10), list(5, 2, 3))

  expect_equal(nextElemOr(it, NA), 32)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), 1000)

  expect_equal(nextElemOr(it, NA), NA)
})

test_that("imap over two lists of unequal length", {
  it <- imap(pow, list(2, 3, 10), list(5, 2, 3, 42))

  expect_equal(nextElemOr(it, NA), 32)
  expect_equal(nextElemOr(it, NA), 9)
  expect_equal(nextElemOr(it, NA), 1000)

  expect_equal(nextElemOr(it, NA), NA)
})

test_that("imap language objects", {
  it <- imap(deparse, alist(foo, bar(baz), qux))

  expect_equal(nextElemOr(it, NA), "foo")
  expect_equal(nextElemOr(it, NA), c("bar(baz)"))
  expect_equal(nextElemOr(it, NA), c("qux"))

})
