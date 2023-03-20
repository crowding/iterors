test_that("test01", {
    actual <- as.list(igrid(x = 1:3, a = letters[1:2], b = 1))
    expected <- list(list(x = 1L, a = "a", b = 1), list(x = 1L,
        a = "b", b = 1), list(x = 2L, a = "a", b = 1), list(x = 2L,
        a = "b", b = 1), list(x = 3L, a = "a", b = 1), list(x = 3L,
        a = "b", b = 1))
    expect_equal(expected, actual)
})

test_that("test02", {
    it <- igrid(a = LETTERS[1:10], b = 1, x = 1:3)
    results <- foreach(a = LETTERS[1:10], .combine = "c") %:%
        foreach(b = 1, .combine = "c") %:% foreach(x = 1:3, actual = it,
        .combine = "c") %do% identical(list(a = a, b = b, x = x),
        actual)
    expect_true(length(results) == 30)
    expect_true(all(results))
})

test_that("test03", {
    it <- igrid(a = iteror(LETTERS[1:10]), b = iteror(1), x = iteror(1:3))
    results <- foreach(a = LETTERS[1:10], .combine = "c") %:%
        foreach(b = 1, .combine = "c") %:% foreach(x = 1:3, actual = it,
        .combine = "c") %do% identical(list(a = a, b = b, x = x),
        actual)
    expect_true(length(results) == 30)
    expect_true(all(results))
})

test_that("test04", {
    actual <- as.list(igrid())
    expected <- list()
    expect_equal(expected, actual)
})

test_that("test05", {
    x <- rnorm(100)
    actual <- unlist(as.list(igrid(x)), use.names = FALSE)
    expect_equal(x, actual)
})

test_that("test06", {
    actual <- as.list(igrid(NULL))
    expect_true(is.list(actual))
    expect_true(length(actual) == 0)
})

test_that("test07", {
    x <- 1:3
    y <- rnorm(5)
    z <- letters[1:4]
    it <- igrid(a = x, b = y, c = z)
    actual <- foreach(d = it, .combine = "rbind") %do% {
        as.data.frame(d, stringsAsFactors = FALSE)
    }
    expected <- expand.grid(c = z, b = y, a = x, stringsAsFactors = FALSE)[3:1]
    expect_equal(expected, actual)
})
