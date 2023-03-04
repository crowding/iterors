test_that("test01", {
    x <- rnorm(1000)
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 
        1000, 1001)) {
        y <- do.call("c", as.list(isplitVector(x, chunks = chunks)))
        expect_equal(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 
        1000, 1001)) {
        y <- do.call("c", as.list(isplitVector(x, chunkSize = chunkSize)))
        expect_equal(y, x)
    }
})

test_that("test02", {
    x <- as.list(rnorm(1000))
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 
        1000, 1001)) {
        y <- do.call("c", as.list(isplitVector(x, chunks = chunks)))
        expect_equal(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 
        1000, 1001)) {
        y <- do.call("c", as.list(isplitVector(x, chunkSize = chunkSize)))
        expect_equal(y, x)
    }
})

test_that("test03", {
    x <- as.character(rnorm(1000))
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 
        1000, 1001)) {
        y <- do.call("c", as.list(isplitVector(x, chunks = chunks)))
        expect_equal(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 
        1000, 1001)) {
        y <- do.call("c", as.list(isplitVector(x, chunkSize = chunkSize)))
        expect_equal(y, x)
    }
})

