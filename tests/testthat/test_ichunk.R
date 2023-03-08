test_that("test01", {
    x <- 1:10
    actual <- as.list(ichunk(x, 3))
    expected <- list(as.list(1:3), as.list(4:6), as.list(7:9),
        as.list(10))
    expect_equal(expected, actual)
})

test_that("test02", {
    n <- 13
    chunkSize <- 7
    x <- rnorm(n * chunkSize)
    actual <- as.list(ichunk(x, chunkSize))
    expect_true(length(actual) == n)
    for (chunk in actual) {
        expect_true(length(chunk) == chunkSize)
    }
    y <- unlist(actual)
    expect_equal(y, x)
})
