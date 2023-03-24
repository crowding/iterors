context("window iterator")

`%is%` <- expect_equal

test_that("iwindow functions properly with an iterator", {
  it <- iteror(letters[1:4])
  it_iwindow <- iwindow(it, 2)
  expect_equal(nextOr(it_iwindow, NA), list("a", "b"))
  expect_equal(nextOr(it_iwindow, NA), list("b", "c"))
  expect_equal(nextOr(it_iwindow, NA), list("c", "d"))
  expect_equal(nextOr(it_iwindow, NA), NA)
})

test_that("iwindow functions properly with a vector", {
  it_iwindow <- iwindow(letters[1:5], 2)
  expect_equal(nextOr(it_iwindow, NA), list("a", "b"))
  expect_equal(nextOr(it_iwindow, NA), list("b", "c"))
  expect_equal(nextOr(it_iwindow, NA), list("c", "d"))
  expect_equal(nextOr(it_iwindow, NA), list("d", "e"))
  expect_equal(nextOr(it_iwindow, NA), NA)
})

test_that("iwindow with tails", {
  it_iwindow <- iwindow(letters[1:5], 2, TRUE)
  expect_equal(nextOr(it_iwindow, NA), list(NULL, "a"))
  expect_equal(nextOr(it_iwindow, NA), list("a", "b"))
  expect_equal(nextOr(it_iwindow, NA), list("b", "c"))
  expect_equal(nextOr(it_iwindow, NA), list("c", "d"))
  expect_equal(nextOr(it_iwindow, NA), list("d", "e"))
  expect_equal(nextOr(it_iwindow, NA), list("e", NULL))
  expect_equal(nextOr(it_iwindow, NA), NA)
})

test_that("iwindow 3-wide functions properly with an iterator", {
  it <- iteror(letters[1:4])
  it_iwindow <- iwindow(it, 3)
  expect_equal(nextOr(it_iwindow, NA), list("a", "b", "c"))
  expect_equal(nextOr(it_iwindow, NA), list("b", "c", "d"))
  expect_equal(nextOr(it_iwindow, NA), NA)
})

test_that("iwindow 3-wide functions properly with a vector", {
  it_iwindow <- iwindow(letters[1:5], 3)
  expect_equal(nextOr(it_iwindow, NA), list("a", "b", "c"))
  expect_equal(nextOr(it_iwindow, NA), list("b", "c", "d"))
  expect_equal(nextOr(it_iwindow, NA), list("c", "d", "e"))
  expect_equal(nextOr(it_iwindow, NA), NA)
})

test_that("window with tails, short sequence", {
  it <- iwindow(c("a", "b", "c"), 5, TRUE)
  nextOr(it, NA) %is% list(NULL, NULL, NULL, NULL, "a")
  nextOr(it, NA) %is% list(NULL, NULL, NULL, "a", "b")
  nextOr(it, NA) %is% list(NULL, NULL, "a", "b", "c")
  nextOr(it, NA) %is% list(NULL, "a", "b", "c", NULL)
  nextOr(it, NA) %is% list("a", "b", "c", NULL, NULL)
  nextOr(it, NA) %is% list("b", "c", NULL, NULL, NULL)
  nextOr(it, NA) %is% list("c", NULL, NULL, NULL, NULL)
  nextOr(it, NA) %is% NA
})

test_that("window with no tails over short sequence", {
  it <- iwindow(c("a", "b", "c"), 5, FALSE)
  nextOr(it, NA) %is% NA
})
