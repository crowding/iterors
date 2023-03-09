test_that("test01", {
    actual <- as.list(ienumerate(icount(3)))
    index <- 1:3
    expected <- lapply(index, function(i) list(index = i, value = i))
    expect_equal(expected, actual)
})

test_that("test02", {
    actual <- as.list(ienumerate(c()))
    expected <- list()
    expect_equal(expected, actual)
})

test_that("test03", {
    x <- rnorm(100)
    actual <- as.list(ienumerate(x))
    expected <- as.list(izip(index = icount(), value = x))
    expect_equal(expected, actual)
})
