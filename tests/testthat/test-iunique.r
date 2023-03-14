context("iunique iterators")

`%is%` <- expect_equal

test_that("iunique works with numeric vectors", {
  x <- rep(1:5, each=10)
  it_unique <- iunique(x)
  expect_equal(take(it_unique, 5), as.list(1:5))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("iunique works with character vectors", {
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it_unique <- iunique(x)
  expect_equal(take(it_unique, 5), as.list(LETTERS[1:5]))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("iunique works with iterators from numeric vectors", {
  x <- rep(1:5, each=10)
  it <- iteror(rep(x, 2))
  it_unique <- iunique(it)
  expect_equal(take(it_unique, 5), as.list(1:5))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("iunique works with iterators from character vectors", {
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it <- iteror(rep(x, 2))
  it_unique <- iunique(it)
  expect_equal(take(it_unique, 5), as.list(LETTERS[1:5]))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("idedupe works with numeric vectors", {
  x <- rep(1:5, each=10)
  it_unique <- idedupe(x)
  expect_equal(take(it_unique, 5), as.list(1:5))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("idedupe works with character vectors", {
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it_unique <- idedupe(x)
  expect_equal(take(it_unique, 5), as.list(LETTERS[1:5]))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("idedupe works with iterators from numeric vectors", {
  num_reps <- 7
  x <- rep(1:5, each=10)
  it <- iteror(rep(x, num_reps))
  it_unique <- idedupe(it)
  expect_equal(take(it_unique, 5 * num_reps), as.list(rep(1:5, num_reps)))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("idedupe works with iterators from character vectors", {
  num_reps <- 7
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it <- iteror(rep(x, num_reps))
  it_unique <- idedupe(it)
  expect_equal(take(it_unique, 5 * num_reps), as.list(rep(LETTERS[1:5], num_reps)))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("idedupe works with other data types", {

  x <- list(function(x=1) y,
            as.name("hooray"),
            environment(),
            list('a', list(4)),
            x~y)
  it <- idedupe(rep(x, each=5))

  expect_identical(nextOr(it), function(x=1) y)
  expect_identical(nextOr(it),  quote(hooray))
  expect_identical(nextOr(it), environment())
  expect_identical(nextOr(it), list("a", list(4)))
  expect_identical(nextOr(it), x ~ y)
  expect_identical(nextOr(it, NULL), NULL)

})

test_that("iunique", {
  fn <- quote(function(x=1) y)

  x <- list(eval(fn),
            as.name("hooray"),
            environment(),
            list('a', list(4)),
            x~y,
            quote(hooray),
            environment(),
            c(list("a"), list(list(4))),
            x~y,
            eval(fn))

  expect_equal(as.list(iunique(x)),
               list(
                 eval(fn),
                 as.name("hooray"),
                 environment(),
                 list('a', list(4)),
                 x~y
               ))

})
