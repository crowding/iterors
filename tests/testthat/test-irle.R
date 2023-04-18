`%is%` <- expect_equal


test_that("irle", {

  target <- lapply(1:10, \(x) list(length=x, value=x))

  result <- (
    as.list(
      irle(
        icollapse(
          iapply(
            icount(10),
            \(x) irep.times(x, x))))))

  result %is% target

  inv_target <- lapply(1:10, \(x) rep(x, x)) |> c(recursive = TRUE)
  inv_result <- as.numeric(irleinv(result))

  inv_result %is% inv_target

})
