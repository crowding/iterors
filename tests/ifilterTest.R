test01 <- function() {
  odd <- function(x) x %% 2 == 1
  even <- function(x) x %% 2 == 0

  actual <- as.list(ifilter(odd, 1:10))
  expected <- as.list(Filter(odd, 1:10))
  checkEquals(actual, expected)

  actual <- as.list(ifilterfalse(odd, 1:10))
  expected <- as.list(Filter(even, 1:10))
  checkEquals(actual, expected)
}
