nextElem <- iterators::nextElem
`%is%` <- expect_equal


test_that("test vector iterator creation", {
  expect_silent(x <- iteror(1:10))
})

test_that("test hasNext, nextElem", {
  x <- iteror(1:10)
  expect_equal(nextElem(x), 1)
  for(i in 1:9) nextElem(x)
  expect_error(nextElem(x))
})

test_that("test matrix iterator creation", {
  x <- matrix(1:10,ncol=2)
  expect_silent(i <- iteror(x, by='cell'))
})

test_that("test hasNext, nextElem", {
  x <- matrix(1:10,ncol=2)
  # by cell
  y <- iteror(x, by='cell')
  expect_equal(nextElem(y), 1)
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iteror(x,by='column')
  expect_equal(nextElem(y), matrix(1:5, ncol=1))
  nextElem(y)
  expect_error(nextElem(y))

  # by col, dropping
  y <- iteror(x,by='column',drop=TRUE)
  expect_equal(nextElem(y), 1:5)
  nextElem(y)
  expect_error(nextElem(y))

  # by row
  y <- iteror(x,by='row')
  expect_equal(nextElem(y), matrix(c(1,6),nrow=1))
  for(i in 1:4) nextElem(y)
  expect_error(nextElem(y))
})



test_that("test data frame iterator creation", {
  x <- data.frame(1:10, 11:20)
  expect_silent(y <- iteror(x))
})
test_that("test hasNext, nextElem", {
  x <- data.frame(1:10, 11:20)
  # by row
  y <- iteror(x, by='row')
  expect_equal(nextElem(y), x[1,])
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iteror(x, by='column')
  expect_equal(nextElem(y), x[,1])
  nextElem(y)
  expect_error(nextElem(y))
})

test_that("empty data frames", {

  x <- as.data.frame(list(a=c(1), b=c(2)))
  emp <- x[c(),]

  it <- iteror(emp, by="row")
  nextOr(it, NA) %is% NA

  it <- iteror(emp, by="col")
  nextOr(it, NA) %is% numeric(0)
  nextOr(it, NA) %is% numeric(0)
  nextOr(it, NA) %is% NA

  emp2 <- data.frame()
  it <- iteror(emp2, by="col")
  nextOr(it, NA) %is% NA

})

# test function iterator creation
# we need to test a function that takes no arguement as
test_that("test hasNext, nextElem", {
  noArgFunc <- function(or) 1
  needArgFunc <- function(i) if(i>100)      stop('too high')    else      i
  y <- iteror(noArgFunc)
  expect_equal(nextElem(y), 1)
  nextElem(y)

  if(FALSE) {
    y <- iteror(needArgFunc)
    expect_equal(nextElem(y), 1)
    for (i in 1:99) nextElem(y)
    expect_error(nextElem(y))
  }
})
