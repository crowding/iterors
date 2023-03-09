context("ichunk iterator")

test_that("ichunk functions properly without using default value", {
  it <- iteror(letters[1:3])
  it_chunk <- ichunk(it, size=3)
  expect_equal(nextOr(it_chunk, NULL), as.list(letters[1:3]))
})

test_that("ichunk does not fill if fill value unspecified", {
  it <- iteror(letters[1:3])
  it_chunk <- ichunk(it, size=2)
  expect_equal(nextOr(it_chunk, NULL), as.list(letters[1:2]))
  expect_equal(nextOr(it_chunk, NULL), list(letters[3]))
})

test_that("ichunk functions properly when fill value is specified", {
  it <- iteror(letters[1:3])
  it_chunk <- ichunk(it, size=2, fill='weeeeee')
  expect_equal(nextOr(it_chunk, NULL), as.list(letters[1:2]))
  expect_equal(nextOr(it_chunk, NULL), list(letters[3], 'weeeeee'))
})

test_that("ichunk rejects non-positive or non-numeric n", {
  it <- iteror(letters)
  error_msg <- "size' must be a positive number of length 1"
  expect_error(ichunk(it, size=-1), error_msg)
  expect_error(ichunk(it, "1"), error_msg)
})

test_that("non-integer size", {
  it <- ichunk(letters, 3.5, "character")
  pasta <- imap(it, paste0, collapse="")
  result <- as.vector(pasta, "character")
})
