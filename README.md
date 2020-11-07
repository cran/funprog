
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Badges -->

[![CRAN\_Status](http://www.r-pkg.org/badges/version/funprog)](https://cran.r-project.org/package=funprog)
[![coverage
report](https://gitlab.com/py_b/funprog/badges/master/coverage.svg)](https://gitlab.com/py_b/funprog/-/commits/master)

# funprog

The *funprog* package provides high-order functions for data
manipulation (grouping, sorting, …). It takes its inspiration from other
pure **fun**ctional **prog**ramming languages (hence the name).

## Installation

You can install *funprog* from the CRAN with :

``` r
install.packages("funprog")
```

To get the development version, you can install it from gitlab :

``` r
# install.packages("remotes")
remotes::install_gitlab("py_b/funprog")
```

## Main functions

Currently, the main functions are all inspired by
[Haskell](https://wiki.haskell.org/Haskell) functions.

### `group_if`

`group_if` splits a vector or a list into groups, given a predicate
function.

The predicate is a binary function returning a boolean, applied to every
couple of adjacent elements. If it evaluates to `TRUE`, those elements
belong to the same group, otherwise they belong to different groups.

Using the equality as a predicate is frequent, therefore `group_eq` is a
shortcut defined as ``group_if(x, `==`)`` for atomic vectors and
`group_if(x, identical)` for other types.

``` r
x1 <- c(2, 4, 2, 2, 1, 1, 1, 3)

str(group_eq(x1)) # shortcut for group_if(x1, `==`)
#> List of 5
#>  $ : num 2
#>  $ : num 4
#>  $ : num [1:2] 2 2
#>  $ : num [1:3] 1 1 1
#>  $ : num 3

str(group_if(x1, `<=`)) # returns non decreasing sequences
#> List of 3
#>  $ : num [1:2] 2 4
#>  $ : num [1:2] 2 2
#>  $ : num [1:4] 1 1 1 3

str(group_if(x1, function(x, y) abs(x - y) > 1))
#> List of 5
#>  $ : num [1:3] 2 4 2
#>  $ : num 2
#>  $ : num 1
#>  $ : num 1
#>  $ : num [1:2] 1 3
```

> *`group_if` is inspired by Haskell’s **groupBy** function.*

### `%on%`

`%on%` is a convenient operator that combines a binary function and a
unary function to create a binary function.

Formally, `(f %on% g)(x, y)` is defined as `f(g(x), g(y))`. For instance
:

``` r
(max %on% abs)(-2, 1) # max(abs(-2), abs(1))
#> [1] 2
```

It may be helpful to create a easy-to-read predicates, particularly in
conjunction with `group_if` :

``` r
x2 <- c(2, 4, 2, -2, -1, 1, 1, 3)
str(group_if(x2, `==` %on% abs))
#> List of 5
#>  $ : num 2
#>  $ : num 4
#>  $ : num [1:2] 2 -2
#>  $ : num [1:3] -1 1 1
#>  $ : num 3

x3 <- list(1:3, 1:3, 3:5, 1, 2)
str(group_if(x3, `==` %on% length))
#> List of 2
#>  $ :List of 3
#>   ..$ : int [1:3] 1 2 3
#>   ..$ : int [1:3] 1 2 3
#>   ..$ : int [1:3] 3 4 5
#>  $ :List of 2
#>   ..$ : num 1
#>   ..$ : num 2
```

> *`%on%` is inspired by Haskell’s **on** function.*

### `sort_by`

`sort_by` sorts a vector or a list, not on values itselves, but on a
transformation of those values.

``` r
sort_by(-3:2, abs)
#> [1]  0 -1  1 -2  2 -3
```

Additional functions can be used for breaking ties, as well as
decreasing order.

``` r
str(sort_by(list(1:2, 3:4, 5), length, descending(sum)))
#> List of 3
#>  $ : num 5
#>  $ : int [1:2] 3 4
#>  $ : int [1:2] 1 2
```

> *`sort_by` is inspired by Haskell’s **sortBy** function.*

## A more elaborate example

`sort_by` and `group_if` can work together well to perform grouping
operations. The next example row-binds data.frames sharing the same
column names :

``` r
library(dplyr)

dfs <- list(
  data.frame(A = 0:1, B = c("a", "b")),
  data.frame(C = 3, D = 4, E = 5),
  data.frame(A = 1),
  data.frame(A = 3, B = "c"),
  data.frame(C = 5:6, D = 7:8, E = 10:11)
)

dfs %>%
  # sort_by to make data.frames sharing same names adjacent
  sort_by(function(x) paste(names(x), collapse = "#")) %>%
  # group_if on identical column names
  group_if(identical %on% names) %>%
  # concatenate data.frames belonging to the same group
  lapply(bind_rows)
#> [[1]]
#>   A
#> 1 1
#> 
#> [[2]]
#>   A B
#> 1 0 a
#> 2 1 b
#> 3 3 c
#> 
#> [[3]]
#>   C D  E
#> 1 3 4  5
#> 2 5 7 10
#> 3 6 8 11
```

## Short syntax with purrr

If you have installed the [purrr](https://purrr.tidyverse.org) package,
you can use the shortcut syntax for specifying arguments that are
functions :

``` r
x1 <- c(2, 4, 2, 2, 1, 1, 1, 3)
str(group_if(x1, ~ abs(.x - .y) > 1))
#> List of 5
#>  $ : num [1:3] 2 4 2
#>  $ : num 2
#>  $ : num 1
#>  $ : num 1
#>  $ : num [1:2] 1 3

x4 <- list(c(a = 1, b = 2, c = 3), c(a = 10, b = 20))
sort_by(x4, "b") # shortcut for `function(x) x[["b"]]`
#> [[1]]
#> a b c 
#> 1 2 3 
#> 
#> [[2]]
#>  a  b 
#> 10 20
sort_by(x4, descending(1)) # shortcut for `descending(function(x) x[[1]])`
#> [[1]]
#>  a  b 
#> 10 20 
#> 
#> [[2]]
#> a b c 
#> 1 2 3
```
