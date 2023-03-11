context("irep iterator")

test_that("irep matches first example from base::rep", {
  it <- irep(1:4, 2)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), NA)
})

test_that("irep matches second example from base::rep", {
  it <- irep(1:4, each=2)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), NA)
})

test_that("irep matches fifth example from base::rep", {
  it <- irep(1:4, each=2, length.out=4)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), NA)
})

test_that("irep replicates a list and matches tenth example from base::rep", {
  # 8 integers plus two recycled 1's.
  fred <- list(happy=1:10, name="squash")
  it <- irep(fred, times=5)
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), NA)
})

test_that("irep replicates a factor and matches last example from base::rep", {
  # 8 integers plus two recycled 1's.
  x <- factor(LETTERS[1:4])
  it <- irep(x, 2)
  expect_equal(nextOr(it, NA), x[1])
  expect_equal(nextOr(it, NA), x[2])
  expect_equal(nextOr(it, NA), x[3])
  expect_equal(nextOr(it, NA), x[4])
  expect_equal(nextOr(it, NA), x[1])
  expect_equal(nextOr(it, NA), x[2])
  expect_equal(nextOr(it, NA), x[3])
  expect_equal(nextOr(it, NA), x[4])
  expect_equal(nextOr(it, NA), NA)
})

test_that("irep_len works on numeric vectors", {
  it <- irep(1:4, length.out=3)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), NA)
})

# Related to Issue #33
test_that("irep matches base::rep() when both times and each args are given", {
  it <- irep(1:4, times=2, each=3)
  expected_vector <- rep(1:4, times=2, each=3)
  expect_equal(unlist(as.list(it)), expected_vector)
})
