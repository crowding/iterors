test_that("izip properly iterates through two numeric vectors", {
  it <- izip(1:3, 4:6)
  expect_equal(nextOr(it, NA), list(1, 4))
  expect_equal(nextOr(it, NA), list(2, 5))
  expect_equal(nextOr(it, NA), list(3, 6))
  expect_equal(nextOr(it, NA), NA)
})

test_that("izip empty", {
  expect_error(izip(), "argument")
  expect_error(izip_longest(), "argument")
  expect_equal(nextOr(izip(list()), "none"), "none")
})

test_that("izip properly iterates through three numeric vectors", {
  it <- izip(1:3, 4:6, 7:9)
  expect_equal(nextOr(it, NA), list(1, 4, 7))
  expect_equal(nextOr(it, NA), list(2, 5, 8))
  expect_equal(nextOr(it, NA), list(3, 6, 9))
  expect_equal(nextOr(it, NA), NA)
})

test_that("izip properly iterates through three numeric vectors and one has extra values", {
  it <- izip(1:3, 4:6, 7:10)
  expect_equal(nextOr(it, NA), list(1, 4, 7))
  expect_equal(nextOr(it, NA), list(2, 5, 8))
  expect_equal(nextOr(it, NA), list(3, 6, 9))
  expect_equal(nextOr(it, NA), NA)
})

test_that("izip properly iterates through two numeric vectors and a character vector", {
  it <- izip(1:3, 4:10, levels(iris$Species))
  expect_equal(nextOr(it, NA), list(1, 4, "setosa"))
  expect_equal(nextOr(it, NA), list(2, 5, "versicolor"))
  expect_equal(nextOr(it, NA), list(3, 6, "virginica"))
  expect_equal(nextOr(it, NA), NA)
})

test_that("izip properly iterates through a numeric vector, a character vector, and a data.frame's columns", {
  it <- izip(1:3, levels(iris$Species), iris)
  expect_equal(nextOr(it, NA), list(1, "setosa", iris[, 1]))
  expect_equal(nextOr(it, NA), list(2, "versicolor", iris[, 2]))
  expect_equal(nextOr(it, NA), list(3, "virginica", iris[, 3]))
  expect_equal(nextOr(it, NA), NA)
})

  test_that("test01", {
    actual <- as.list(izip(a = 1, b = 2, c = rep(3, 100)))
    expected <- list(list(a = 1, b = 2, c = 3))
    expect_equal(expected, actual)
})

test_that("test03", {
    x <- 1.0:3
    y <- rnorm(5)
    z <- letters[1:4]
    nx <- length(x)
    ny <- length(y)
    nz <- length(z)
    ix <- irep(x, each = ny * nz)
    iy <- irep(y, times = nx, each = nz)
    iz <- irep(z, times = nx * ny)
    actual <- as.list(izip(a = ix, b = iy, c = iz))
    expected <- as.list(igrid(a = as.list(x),
                              b = y, c = z, rowMajor=FALSE))
    expect_equal(expected, actual)
})
