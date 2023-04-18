test_that("as.vector concatenation option", {

  for (i in 0:38) {
    expected <- as.numeric(do.call("c", lapply(seq_len(i), seq_len)))
    it <- iapply(icount(i), seq_len)
    actual <- as.numeric(it, collapse=TRUE, n=75)
    rest <- as.numeric(it, collapse=TRUE)
    expect_equal(expected, c(actual, rest))
  }

})

test_that("conversions", {
  expect_equal(as.character(iteror(letters)), letters)
  expect_equal(as.numeric(icount(10)), seq_len(10))
  expect_equal(as.double(icount(10)), seq_len(10))
  expect_equal(as.list(icount(10)), as.list(seq_len(10)))
  expect_equal(as.logical(iteror(c(TRUE, FALSE))),
               c(TRUE, FALSE))
  expect_equal(collect(rep(NULL, 10), collapse=TRUE),
               list())
})
