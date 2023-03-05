test_that("test01", {
    x <- as.list(rnorm(10))
    each <- 2
    actual <- as.list(irep(x, each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
})

test_that("test02", {
    x <- as.list(rnorm(10))
    times <- 2
    actual <- as.list(irep(x, times = times))
    expected <- rep(x, times = times)
    expect_equal(expected, actual)
    times <- rep(2, length(x))
    actual <- as.list(irep(x, times = times))
    expected <- rep(x, times = times)
    expect_equal(expected, actual)
})

test_that("test03", {
    x <- as.list(rnorm(10))
    times <- 2
    actual <- as.list(irep(x, times))
    expected <- rep(x, times)
    expect_equal(expected, actual)
    times <- rep(2, length(x))
    actual <- as.list(irep(x, times))
    expected <- rep(x, times)
    expect_equal(expected, actual)
})

test_that("test04", {
    x <- as.list(rnorm(10))
    length.out <- 33
    actual <- as.list(irep(x, length.out = length.out))
    expected <- rep(x, length.out = length.out)
    expect_equal(expected, actual)
})

test_that("test05", {
    x <- as.list(rnorm(10))
    length.out <- 33
    actual <- as.list(irep(x, each = 2, length.out = length.out))
    expected <- rep(x, each = 2, length.out = length.out)
    expect_equal(expected, actual)
})

test_that("test06", {
    x <- as.list(rnorm(10))
    each <- 2
    times <- 3
    actual <- as.list(irep(x, each = each, times = times))
    expected <- rep(x, each = each, times = times)
    expect_equal(expected, actual)
})

test_that("test07", {
    x <- as.list(rnorm(10))
    each <- 2
    times <- seq(length = length(x) * each)
    actual <- as.list(irep(x, each = each, times = times))
    expected <- rep(rep(x, each = each), times = times)
    expect_equal(expected, actual)
})

test_that("test08", {
    expect_error(irep(1:3, times = integer()))
})

test_that("test09", {
    expect_error(irep(1:3, times = -1))
})

test_that("test10", {
    expect_error(irep(1:3, times = c(1, 1, -1)))
})

test_that("test11", {
    x <- as.list(rnorm(10))
    actual <- as.list(irep(x))
    expected <- rep(x)
    expect_equal(expected, actual)
})

test_that("test12", {
    x <- list()
    times <- 100
    actual <- as.list(irep(x, times = times))
    expected <- rep(x, times = times)
    expect_equal(expected, actual)
    each <- 10
    actual <- as.list(irep(x, each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
    actual <- as.list(irep(x, times = times, each = each))
    expected <- rep(x, times = times, each = each)
    expect_equal(expected, actual)
})

test_that("test13", {
    x <- list()
    times <- 100
    actual <- as.list(irep(iteror(x), times = times))
    expected <- rep(x, times = times)
    expect_equal(expected, actual)
    each <- 10
    actual <- as.list(irep(iteror(x), each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
    actual <- as.list(irep(iteror(x), times = times, each = each))
    expected <- rep(x, times = times, each = each)
    expect_equal(expected, actual)
})
