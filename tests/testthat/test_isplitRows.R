test_that("test01", {
    x <- matrix(rnorm(10000), 100)
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
        y <- do.call("rbind", as.list(isplitRows(x, chunks = chunks)))
        expect_equal(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
        y <- do.call("rbind", as.list(isplitRows(x, chunkSize = chunkSize)))
        expect_equal(y, x)
    }
})

