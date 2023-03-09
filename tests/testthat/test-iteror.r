`%is%` <- expect_equal

test_that("hasNext", {
  i <- ihasNext(iseq(1, 10))
  for (j in 1:10) {
    hasNext(i) %is% TRUE
    nextElem(i) %is% j
  }
  hasNext(i) %is% FALSE
  expect_error(nextElem(i), "StopIteration")
})
