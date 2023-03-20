test_that("test01", {
    actual <- as.list(izip(a = 1, b = 2, c = rep(3, 100)))
    expected <- list(list(a = 1, b = 2, c = 3))
    expect_equal(expected, actual)
})

test_that("test02", {
    actual <- as.list(izip())
    expected <- list()
    expect_equal(expected, actual)
})

test_that("test03", {
    x <- 1:3
    y <- rnorm(5)
    z <- letters[1:4]
    nx <- length(x)
    ny <- length(y)
    nz <- length(z)
    ix <- irep(x, each = ny * nz)
    iy <- irep(y, times = nx, each = nz)
    iz <- irep(z, times = nx * ny)
    actual <- as.list(izip(a = ix, b = iy, c = iz))
    expected <- as.list(igrid(a = x, b = y, c = z))
    expect_equal(expected, actual)
})
