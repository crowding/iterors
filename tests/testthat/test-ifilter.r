test_that("Apply ikeep to an integer sequence", {
  is_even <- function(x) {
    x %% 2 == 0
  }
  it <- ikeep(1:10, is_even)

  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), 8)
  expect_equal(nextOr(it, NA), 10)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply ikeep to an integer sequence using anonymous function", {
  it <- ikeep(1:10, function(x) x %% 2 == 1)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 7)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply ikeep to a character vector", {
  is_vowel <- function(x) {
    x %in% c('a', 'e', 'i', 'o', 'u')
  }
  it <- ikeep(letters, is_vowel)

  expect_equal(nextOr(it, NA), 'a')
  expect_equal(nextOr(it, NA), 'e')
  expect_equal(nextOr(it, NA), 'i')
  expect_equal(nextOr(it, NA), 'o')
  expect_equal(nextOr(it, NA), 'u')
  expect_equal(nextOr(it, NA), NA)
})
