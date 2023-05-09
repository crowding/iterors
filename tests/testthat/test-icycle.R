test_that("Indefinite i_recycle of integer sequence", {
  it <- i_recycle(1:3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
})

test_that("i_recycle repeats integer fixed number of times", {
  it <- i_recycle(1:3, times=2)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), NA)
})


test_that("i_recycle repeats indefinitely for a function", {
  it <- i_recycle(function() rnorm(1))

  set.seed(1)
  i <- nextOr(it, NA)
  set.seed(1)
  expect_equal(i, rnorm(1))

  set.seed(2)
  i <- nextOr(it, NA)
  set.seed(2)
  expect_equal(i, rnorm(1))

  set.seed(3)
  i <- nextOr(it, NA)
  set.seed(3)
  expect_equal(i, rnorm(1))
})

test_that("i_recycle repeats a fixed number of times for a function", {
  it <- i_recycle(function() rnorm(1), times=3)

  set.seed(1)
  i <- nextOr(it, NA)
  set.seed(1)
  expect_equal(i, rnorm(1))

  set.seed(2)
  i <- nextOr(it, NA)
  set.seed(2)
  expect_equal(i, rnorm(1))

  set.seed(3)
  i <- nextOr(it, NA)
  set.seed(3)
  expect_equal(i, rnorm(1))

  expect_equal(nextOr(it, NA), NA)
})
