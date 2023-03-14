nextElem <- iterators::nextElem

# test vector iterator creation
test_that( {
  x <- iter(1:10)
})

# test hasNext, nextElem
test_that( {
  x <- iter(1:10)
  expect_equal(nextElem(x), 1)
  for(i in 1:9) nextElem(x)
  expect_error(nextElem(x))
})

# test hasNext, nextElem
test_that( {
  x <- iter(1:10)
  expect_equal(nextElem(x), 1)
  for(i in 1:9) nextElem(x)
  expect_error(nextElem(x))
})

# check checkFunc
test_that( {
  x <- iter(1:100, checkFunc=function(i) i%%10==0)
  expect_equal(nextElem(x), 10)
  for(i in 1:9) nextElem(x)
  expect_error(nextElem(x))
})

# test matrix iterator creation
test_that( {
  x <- matrix(1:10,ncol=2)
})

# test hasNext, nextElem
test_that( {
  x <- matrix(1:10,ncol=2)
  # by cell
  y <- iter(x,by='cell')
  expect_equal(nextElem(y), 1)
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iter(x,by='column')
  expect_equal(nextElem(y), matrix(1:5, ncol=1))
  nextElem(y)
  expect_error(nextElem(y))

  # by row
  y <- iter(x,by='row')
  expect_equal(nextElem(y), matrix(c(1,6),nrow=1))
  for(i in 1:4) nextElem(y)
  expect_error(nextElem(y))
})

# test checkFunc
test_that( {
  # create a larger matrix
  x <- matrix(1:100, ncol=20)

  # by cell
  y <- iter(x, by='cell', checkFunc=function(i) i%%10==0)
  expect_equal(nextElem(y), 10)
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iter(x, by='column', checkFunc=function(i) i[5]%%10==0)
  expect_equal(nextElem(y), as.matrix(x[,2]))
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by row
  # create an easier matrix to deal with
  x <- matrix(1:100, nrow=20, byrow=TRUE)
  y <- iter(x, by='row', checkFunc=function(i) i[5]%%10==0)
  expect_equal(as.vector(nextElem(y)), x[2,])
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))
})

# test data frame iterator creation
test_that( {
  x <- data.frame(1:10, 11:20)
  y <- iter(x)
})
# test hasNext, nextElem
test_that( {
  x <- data.frame(1:10, 11:20)
  # by row
  y <- iter(x, by='row')
  expect_equal(nextElem(y), x[1,])
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iter(x, by='column')
  expect_equal(nextElem(y), x[,1])
  nextElem(y)
  expect_error(nextElem(y))
})

# test checkFunc
test_that( {
  x <- data.frame(1:10, 11:20)
  # by row
  y <- iter(x, by='row', checkFunc=function(i) i[[1]][1]%%2==0)
  expect_equal(nextElem(y),x[2,])
  for(i in 1:4) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iter(x, by='column', checkFunc=function(i) i[[1]][1]%%11==0)
  expect_equal(nextElem(y), x[,2])
  expect_error(nextElem(y))
})

# test function iterator creation
# we need to test a function that takes no arguement as
# well as one that takes the index
test_that( {
  noArgFunc <- function() 1
  needArgFunc <- function(i)
    if(i>100)
      stop('too high')
    else
      i
})

# test hasNext, nextElem
test_that( {
  noArgFunc <- function() 1
  needArgFunc <- function(i) if(i>100)      stop('too high')    else      i
  y <- iter(noArgFunc)
  expect_equal(nextElem(y), 1)
  nextElem(y)

  y <- iter(needArgFunc)
  expect_equal(nextElem(y), 1)
  for (i in 1:99) nextElem(y)
  expect_error(nextElem(y))
})

# test checkFunc
test_that( {
  noArgFunc <- function() 1
  needArgFunc <- function(i)
    if(i>100)
      stop('too high')
    else
      i
  y <- iter(noArgFunc, checkFunc=function(i) i==1)
  expect_equal(nextElem(y), 1)
  nextElem(y)

  y <- iter(needArgFunc, checkFunc=function(i) i%%10==0)
  expect_equal(nextElem(y), 10)
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))
})
