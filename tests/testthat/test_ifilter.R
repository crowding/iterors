test_that("test01", {
    odd <- function(x) x%%2 == 1
    even <- function(x) x%%2 == 0
    actual <- as.list(ikeep(1:10, odd))
    expected <- as.list(Filter(odd, 1:10))
    expect_equal(expected, actual)
    actual <- as.list(idrop(1:10, odd))
    expected <- as.list(Filter(even, 1:10))
    expect_equal(expected, actual)
})
