context("icount iterator: Infinite sequence")

test_that("icount's default values generate the sequence 0, 1, 2, ..", {
  it <- icount()

  i <- nextElemOr(it, NULL)
  expect_equal(i, 0)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 1)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 2)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 3)
})

test_that("icount works with a given initial value", {
  it <- icount(start=42)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 42)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 43)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 44)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 45)
})

test_that("icount works with a decimal step size", {
  it <- icount(start=42, step=1.5)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 42)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 43.5)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 45)

  i <- nextElemOr(it, NULL)
  expect_equal(i, 46.5)
})

test_that("icount works with the optional stop", {
  it <- icount(stop=5)
  expect_equal(0:5, unlist(as.list(it)))
})

test_that("icount works with a given initial value and a stop", {
  it <- icount(start=42, stop=50)
  expect_equal(42:50, unlist(as.list(it)))
})

test_that("icount works with a stop and decimal step size", {
  it <- icount(start=42, step=1.5, stop=50)
  expect_equal(seq(42, 50, by=1.5), unlist(as.list(it)))
})
