context("ipairwise iterator")

test_that("ipairwise functions properly with an iterator", {
  it <- iteror(letters[1:4])
  it_ipairwise <- ipairwise(it)
  expect_equal(nextOr(it_ipairwise, NA), list("a", "b"))
  expect_equal(nextOr(it_ipairwise, NA), list("b", "c"))
  expect_equal(nextOr(it_ipairwise, NA), list("c", "d"))
  expect_equal(nextOr(it_ipairwise, NA), NA)
})

test_that("ipairwise functions properly with a vector", {
  it_ipairwise <- ipairwise(letters[1:5])
  expect_equal(nextOr(it_ipairwise, NA), list("a", "b"))
  expect_equal(nextOr(it_ipairwise, NA), list("b", "c"))
  expect_equal(nextOr(it_ipairwise, NA), list("c", "d"))
  expect_equal(nextOr(it_ipairwise, NA), list("d", "e"))
  expect_equal(nextOr(it_ipairwise, NA), NA)
})
