context("ichunk iterator")

test_that("ichunk functions properly without using default value", {
  it <- iteror(letters[1:3])
  it_chunk <- ichunk(it, chunk_size=3)
  expect_equal(nextElemOr(it_chunk, NULL), as.list(letters[1:3]))
})

test_that("ichunk functions properly when using fill value", {
  it <- iteror(letters[1:3])
  it_chunk <- ichunk(it, chunk_size=2)
  expect_equal(nextElemOr(it_chunk, NULL), as.list(letters[1:2]))
  expect_equal(nextElemOr(it_chunk, NULL), list(letters[3], NA))
})

test_that("ichunk functions properly when fill value is specified", {
  it <- iteror(letters[1:3])
  it_chunk <- ichunk(it, chunk_size=2, fill='weeeeee')
  expect_equal(nextElemOr(it_chunk, NULL), as.list(letters[1:2]))
  expect_equal(nextElemOr(it_chunk, NULL), list(letters[3], 'weeeeee'))
})

test_that("ichunk rejects non-positive or non-numeric n", {
  it <- iteror(letters)
  error_msg <- "'chunk_size' must be a positive integer of length 1"
  expect_error(ichunk(it, chunk_size=-1), error_msg)
  expect_error(ichunk(it, "1"), error_msg)
})
