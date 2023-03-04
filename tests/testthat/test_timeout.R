test_that("test01", {
    it <- ihasNext(timeout(icount(), 2))
    x <- 0
    while (hasNext(it)) x <- nextElem(it)
    expect_true(x > 0)
})

test_that("test02", {
    n <- 1000
    actual <- as.list(timeout(icount(n), time = Inf))
    expected <- as.list(1:n)
    expect_equal(expected, actual)
})

