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

test_that("test01", {
    actual <- unlist(as.list(irepeat(42, 10)))
    expected <- rep(42, 10)
    expect_equal(expected, actual)
})

test_that("test02", {
    actual <- as.list(irepeat(42, 0))
    expected <- list()
    expect_equal(expected, actual)
})

test_that("test03", {
    actual <- unlist(as.list(irepeat(42), 10))
    expected <- rep(42, 10)
    expect_equal(expected, actual)
})

test_that("test04", {
    x <- 1:10
    n <- 10
    actual <- as.list(irepeat(x), n)
    expected <- rep(list(x), n)
    expect_equal(expected, actual)
})

test_that("test05", {
    x <- as.list(1:10)
    n <- 10
    actual <- as.list(irepeat(x), n)
    expected <- rep(list(x), n)
    expect_equal(expected, actual)
})
