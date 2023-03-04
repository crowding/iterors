test_that("test01", {
    actual <- as.list(chain(a = 1, b = 2, c = rep(3.14159, 100)))
    expected <- c(list(1, 2), as.list(rep(3.14159, 100)))
    expect_equal(expected, actual)
})

test_that("test02", {
    actual <- as.list(chain())
    expected <- list()
    expect_equal(expected, actual)
})

