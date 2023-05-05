test_that("Iteror from Python object", {
  skip_if_no_python()

  l <- reticulate::r_to_py(list(1, 2, 3))
  obj <- reticulate::as_iterator(l)
  Robj <- iteror(obj)
  lR <- as.list(Robj)
  expect_equal(lapply(lR, reticulate::py_to_r), list(1, 2, 3))

})

test_that("Python object from iteror", {
  skip_if_no_python()

  pit <- py_iteror(iseq(2, 11, 3))
  expect_equal(reticulate::iter_next(pit), 2)
  expect_equal(reticulate::iter_next(pit), 5)
  expect_equal(reticulate::iter_next(pit), 8)
  expect_equal(reticulate::iter_next(pit), 11)
  expect_equal(reticulate::iter_next(pit, "what"), "what")

})
