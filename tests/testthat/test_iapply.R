test_that("iterate over 3D arrays", {

  test <- function(actual, it) {
    expected <- nextOr(it)
    expect_equal(expected, actual)
    NULL
  }

  a <- array(1:24, c(2,3,4))

  margins <- list(1, 2, 3,
                  c(1, 2), c(1, 3), c(2, 1), c(2, 3), c(3, 1), c(3, 2),
                  c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1),
                  c(3, 1, 2), c(3, 2, 1))
  for(MARGIN in margins) {
    # cat(sprintf('testing %s\n', paste(MARGIN, collapse=', ')))
    it <- iteror(a, by=MARGIN, drop=TRUE)
    apply(a, MARGIN, test, it)
  }

})

test_that("iapply on matrices", {
  test <- function(actual, it) {
    expected <- nextOr(it)
    expect_equal(expected, actual)
    NULL
  }

  m <- matrix(1:24, c(6,4))

  margins <- list(1, 2, c(1, 2), c(2, 1))
  for(MARGIN in margins) {
    # cat(sprintf('testing %s\n', paste(MARGIN, collapse=', ')))
    it <- iteror(m, by=MARGIN, drop=TRUE)
    apply(m, MARGIN, test, it)
  }
})
