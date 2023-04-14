test_that("ilength functions properly with a vector", {
  expect_equal(ilength(1:5), 5)
})

test_that("ilength functions properly with an iteror", {
  it <- iteror(1:5)
  expect_equal(ilength(it), 5)
})

test_that("ilength functions properly with a chained iteror", {
  it <- ichain(1:3, 4:5, 6)
  expect_equal(ilength(it), 6)
})

test_that("ilength functions properly with a chained iteror of mixed types", {
  it <- ichain(1:3, levels(iris$Species))
  expect_equal(ilength(it), 6)
})
