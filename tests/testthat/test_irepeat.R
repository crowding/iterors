test_that("test01", {
    actual <- unlist(as.list(irepeat(42, 10)))
    expected <- rep(42, 10)
    expect_equal(expected, actual)
})

test_that("test02", {
    actual <- as.list(irepeat(42, 0))
    expected <- list()
    expect_equal(expected, actual)
})

test_that("test03", {
    actual <- unlist(as.list(irepeat(42), 10))
    expected <- rep(42, 10)
    expect_equal(expected, actual)
})

test_that("test04", {
    x <- 1:10
    n <- 10
    actual <- as.list(irepeat(x), n)
    expected <- rep(list(x), n)
    expect_equal(expected, actual)
})

test_that("test05", {
    x <- as.list(1:10)
    n <- 10
    actual <- as.list(irepeat(x), n)
    expected <- rep(list(x), n)
    expect_equal(expected, actual)
})

