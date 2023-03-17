# iterors

  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/crowding/iterors/branch/master/graph/badge.svg)](https://app.codecov.io/gh/crowding/iterors?branch=master) [![check_full](https://github.com/crowding/iterors/actions/workflows/check_full.yaml/badge.svg)](https://github.com/crowding/iterors/actions/workflows/check_full.yaml) [![pkgdown](https://github.com/crowding/iterors/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/crowding/iterors/actions/workflows/pkgdown.yaml)
  <!-- badges: end -->

A fresh take on iterators in R. Designed to be cross-compatible with the `iterators` package, but using the `nextOr` method will offer better performance and more compact code. With batteries included: includes a collection of iterator constructors and combinators ported and harmonized from the `iterators`, `itertools`, and `itertools2` packages, as well as several new functions.

## How is it different from `iterators`?

`iterors` uses the method `nextOr(it, or)` to retrieve the next element. The lazily evaluated second argument specifies a sigil value _or_ an action to take at the end of iteration. By contrast, `iterators` method `nextElem` takes only one argument and signals end of iteration by throwing an exception.

For example, this is how you can compute a sum over an iteror `it`:

```{R}
total <- 0
repeat total <- total + nextOr(it, break)
```

By contrast, this is how you did this with `iterators`:

```{R}
total <- 0
tryCatch(
  repeat total <- total + nextElem(it),
  error=function(x) {
    if (conditionMessage(x) != "StopIteration") stop(x)
  }
)
```

Besides requiring less boilerplate, iterors also perform faster, particularly when using higher-order iterator functions (which used to require setting up and tearing down a tryCatch for every iteration.)

A third benefit is that because `iterors` does not force you to use exceptions for flow control, debugging code using `iterors` is easier, as your stack traces are no longer swallowed by `tryCatch`.

## Installation

For the time being, run the following after installing [devtools](https://github.com/hadley/devtools):

```r
devtools::install_github('crowding/iterors')
```

When the package is released, you will be able to install the stable version from [CRAN](htxtp://cran.r-project.org/package=iterors):

```r
install.packages('iteror', dependencies=TRUE)
```

## License

 GPL-3
