# iterors

The R package `iterors` is a replacement for both `iterators` and `itertools2`, it uses
a slightly different calling convention that turns out to be more efficient as well as usually resulting in more compact code.

It includes a port of the R package `itertools2` is a port of [Python's excellent itertools
module](https://docs.python.org/2/library/itertools.html) to R for efficient
looping and is a replacement for the existing [itertools R
package](https://r-forge.r-project.org/projects/itertools/).

## Installation

For the time being, run the following after installing [devtools](https://github.com/hadley/devtools):

```r
devtools::install_github('crowding/iterors')
```

When the package is released, you will be able to install the stable version from [CRAN](http://cran.r-project.org/package=iterors):

```r
install.packages('iteror', dependencies=TRUE)
```

## License

The `iterors` R package is licensed under the [MIT
License](http://opensource.org/licenses/MIT).
