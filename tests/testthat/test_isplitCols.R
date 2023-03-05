test_that("test01", {
    x <- matrix(rnorm(10000), 100)
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
        y <- do.call("cbind", as.list(isplitCols(x, chunks = chunks)))
        expect_equal(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
        y <- do.call("cbind", as.list(isplitCols(x, chunkSize = chunkSize)))
        expect_equal(y, x)
    }
})
