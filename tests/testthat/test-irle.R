
`%is%` <- expect_equal


test_that("irle", {

  target <- 1:10 |> lapply(\(x) list(length=x, value=x))

  result <- (
    icount(10)
    |> iapply(\(x) irep.times(x, x))
    |> icollapse()
    |> irle()
    |> as.list())

  result %is% target

  inv_target <- lapply(1:10, \(x) rep(x, x)) |> c(recursive = TRUE)
  inv_result <- result |> irle_inverse() |> as.numeric()

  inv_result %is% inv_target

})
