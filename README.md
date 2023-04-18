# iterors

  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/crowding/iterors/branch/master/graph/badge.svg)](https://app.codecov.io/gh/crowding/iterors?branch=master) [![check_full](https://github.com/crowding/iterors/actions/workflows/check_full.yaml/badge.svg)](https://github.com/crowding/iterors/actions/workflows/check_full.yaml) [![pkgdown](https://github.com/crowding/iterors/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/crowding/iterors/actions/workflows/pkgdown.yaml)
  <!-- badges: end -->

A fresh take on iterators in R. 

## Features

 * Main method `nextOr(iter, or)` allows shorter and simpler code.
 * `iteror` objects are backward- and forward-compatible with existing code using `iterators` (such as the `foreach` package.)
 * Optimized performance, with several times less overhead per item.
 * Comes with a complete collection of iterator functions, ported and curated from packages `iterators`, `itertools`, and `itertools2`, and harmonized with uniform options 

## How is it different from `iterators`?

`iterors` uses the method `nextOr(it, or)` to retrieve the next element. The second argument `or` is lazily evaluated; it can specify a return value _or_ an action to take at the end of iteration. In particular, `or` can be a normal control flow construct like `break` or `next` or `return`. 

For example, this is how you can compute a sum over an iteror `it`:

```{R}
total <- 0
repeat 
  total <- total + nextOr(it, break)
```

To contrast with the existing `iterators` package: In that package `nextElem` signals end of iteration by throwing an exception. Computing a sum over an iterator looked like this:

```{R}
total <- 0
tryCatch(
  repeat total <- total + nextElem(it),
  error=function(x) {
    if (conditionMessage(x) != "StopIteration") stop(x)
  }
)
```

Besides requiring less boilerplate, iterator code written using `nextOr` also performs faster, particularly when using higher-order iterator functions. This is because `tryCatch` is a relatively expensive operation in R, especially when used once per item. It is also not possible to use `break` or `next` to break an outer loop from inside a `tryCatch` handler.

The [benchmarking][] vignette illustrates that iterator computations using  `iterors` can execute several times faster than using predecessors.



## Installation

For the time being, run the following after installing [devtools](https://github.com/hadley/devtools):

```{R, eval=FALSE}
devtools::install_github('crowding/iterors')
```

When the package is released, you will be able to install the stable version from [CRAN](htxtp://cran.r-project.org/package=iterors):

```{R, eval=FALSE}
install.packages('iteror', dependencies=TRUE)
```

## License

GPL-3
