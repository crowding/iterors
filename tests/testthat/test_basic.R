nextElem <- iterators::nextElem

test_that("test vector iterator creation", {
  expect_silent(x <- iteror(1:10))
})

test_that("test hasNext, nextElem", {
  x <- iteror(1:10)
  expect_equal(nextElem(x), 1)
  for(i in 1:9) nextElem(x)
  expect_error(nextElem(x))
})

test_that("check checkFunc", {
  x <- iteror(1:100, checkFunc=function(i) i%%10==0)
  expect_equal(nextElem(x), 10)
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

test_that("test checkFunc", {
  # create a larger matrix
  x <- matrix(1:100, ncol=20)

  # by cell
  y <- iteror(x, by='cell', checkFunc=function(i) i%%10==0)
  expect_equal(nextElem(y), 10)
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iteror(x, by='column', checkFunc=function(i) i[5]%%10==0)
  expect_equal(nextElem(y), as.matrix(x[,2]))
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by row
  # create an easier matrix to deal with
  x <- matrix(1:100, nrow=20, byrow=TRUE)
  y <- iteror(x, by='row', checkFunc=function(i) i[5]%%10==0)
  expect_equal(as.vector(nextElem(y)), x[2,])
  for(i in 1:9) nextElem(y)
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

test_that("test checkFunc", {
  x <- data.frame(1:10, 11:20)
  # by row
  y <- iteror(x, by='row', checkFunc=function(i) i[[1]][1]%%2==0)
  expect_equal(nextElem(y),x[2,])
  for(i in 1:4) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iteror(x, by='column', checkFunc=function(i) i[[1]][1]%%11==0)
  expect_equal(nextElem(y), x[,2])
  expect_error(nextElem(y))
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

test_that("test checkFunc", {
  noArgFunc <- function(or) 1
  needArgFunc <- function(i)
    if(i>100)
      stop('too high')
    else
      i
  y <- iteror(noArgFunc, checkFunc=function(i) i==1)
  expect_equal(nextElem(y), 1)
  nextElem(y)

  if(FALSE) {
    y <- iteror(needArgFunc, checkFunc=function(i) i%%10==0)
    expect_equal(nextElem(y), 10)
    for(i in 1:9) nextElem(y)
    expect_error(nextElem(y))
  }
})
