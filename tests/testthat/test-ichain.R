test_that("ichain properly iterates through multiple numeric vectors", {
  it <- ichain(1:3, 4:5, 6)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), NA)
})

test_that("ichain properly iterates through a numeric vector and a character vector", {
  it <- ichain(1:3, levels(iris$Species))
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), "setosa")
  expect_equal(nextOr(it, NA), "versicolor")
  expect_equal(nextOr(it, NA), "virginica")
  expect_equal(nextOr(it, NA), NA)
})

test_that("ichain properly iterates through a numeric vector and a data.frame's columns", {
  it <- ichain(1:3, ilimit(iris, 5))
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), iris[, 1])
  expect_equal(nextOr(it, NA), iris[, 2])
  expect_equal(nextOr(it, NA), iris[, 3])
  expect_equal(nextOr(it, NA), iris[, 4])
  expect_equal(nextOr(it, NA), iris[, 5])
  expect_equal(nextOr(it, NA), NA)
})

test_that("test01", {
    actual <- as.list(ichain(a = 1, b = 2, c = rep(3.14159, 100)))
    expected <- c(list(1, 2), as.list(rep(3.14159, 100)))
    expect_equal(expected, actual)
})

test_that("test02", {
    actual <- as.list(ichain())
    expected <- list()
    expect_equal(expected, actual)
})
