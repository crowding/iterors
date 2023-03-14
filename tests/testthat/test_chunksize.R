library(iterors)

test_that("test that various values of chunksize", {
  nr <- 13
  nc <- 21
  mat <- matrix(rnorm(nr * nc), nr)

  for (n in 1:(nc+2)) {
    it <- iteror(mat, by='col', chunksize=n)
    bcols <- as.list(it)
    for (bcol in bcols) {
      expect_true(nrow(bcol) == nr)
      expect_true(ncol(bcol) <= n && ncol(bcol) >= 1)
    }
    actual <- do.call('cbind', bcols)
    expect_equal(mat, actual)
  }

  for (n in 1:(nr+2)) {
    it <- iteror(mat, by='row', chunksize=n)
    brows <- as.list(it)
    for (brow in brows) {
      expect_true(ncol(bcol) == nc)
      expect_true(nrow(brow) <= n && nrow(brow) >= 1)
    }
    actual <- do.call('rbind', brows)
    expect_equal(mat, actual)
  }
})
