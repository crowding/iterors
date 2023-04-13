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

test_that("iunique with general data types", {
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
