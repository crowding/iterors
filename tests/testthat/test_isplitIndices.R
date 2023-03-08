test_that("test01", {
    x <- seq(length = 1000)
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999,
        1000, 1001)) {
        y <- do.call("c", as.list(isplitIndices(length(x), chunks = chunks)))
        expect_equal(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999,
        1000, 1001)) {
        y <- do.call("c", as.list(isplitIndices(length(x), chunkSize = chunkSize)))
        expect_equal(y, x)
    }
})
