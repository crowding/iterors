context("itripletwise iterator")

test_that("itripletwise functions properly with an iterator", {
  it <- iteror(letters[1:4])
  it_itripletwise <- itripletwise(it)
  expect_equal(nextElemOr(it_itripletwise, NA), list("a", "b", "c"))
  expect_equal(nextElemOr(it_itripletwise, NA), list("b", "c", "d"))
  expect_equal(nextElemOr(it_itripletwise, NA), NA)
})

test_that("itripletwise functions properly with a vector", {
  it_itripletwise <- itripletwise(letters[1:5])
  expect_equal(nextElemOr(it_itripletwise, NA), list("a", "b", "c"))
  expect_equal(nextElemOr(it_itripletwise, NA), list("b", "c", "d"))
  expect_equal(nextElemOr(it_itripletwise, NA), list("c", "d", "e"))
  expect_equal(nextElemOr(it_itripletwise, NA), NA)
})
