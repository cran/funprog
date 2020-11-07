#' Apply a function repeatedly
#'
#' Apply a function to a value, then reapply the same function to the result and
#' so on... until a condition on the result is met (or a certain number of
#' iterations reached).
#'
#' As it is a very generic function (\code{x} can be any type of object) and the
#' number of computations cannot be known in advance, \code{iterate} can be
#' quite inefficient (particularly if you use \code{accumulate = TRUE}).
#'
#' @param x initial value.
#' @param f the function to apply.
#' @param stop_fun a predicate (function) evaluated on the current result, which
#'   will stop the process if its result is \code{TRUE}. If not provided, the
#'   process will stop after \code{stop_n} iteration (see below).
#' @param stop_n maximal number of times the function will be applied (mandatory
#'   if \code{stop_fun} is not defined).
#' @param accumulate by default, the function returns only the last element. To
#'   get the list of all intermediate results, turn this parameter to
#'   \code{TRUE}.
#'
#' @return The last result, or the list of all results if
#'   \code{accumulate = TRUE}.
#'
#' @export
#'
#' @examples
#' # https://en.wikipedia.org/wiki/Collatz_conjecture
#' syracuse <- function(x) if (x %% 2) 3 * x + 1 else x / 2
#' iterate(
#'   10,
#'   syracuse,
#'   stop_fun = function(n) n == 1,
#'   accumulate = TRUE
#' )
#'
#' # https://en.wikipedia.org/wiki/H%C3%A9non_map
#' henon_attractor <-
#'   iterate(
#'     c(-1, 0.1),
#'     function(x) c(1 - 1.4 * x[1]^2 + x[2], 0.3 * x[1]),
#'     stop_n = 5000,
#'     accumulate = TRUE
#'   )
#' plot(
#'   sapply(henon_attractor, function(.) .[1]),
#'   sapply(henon_attractor, function(.) .[2]),
#'   pch = "."
#' )

iterate <- function(x,
                    f,
                    stop_fun = NULL,
                    stop_n = Inf,
                    accumulate = FALSE) {

  force(f)
  force(stop_fun)

  if (accumulate) acc <- list(x)

  stopifnot(is.infinite(stop_n) || as.integer(stop_n) > 0)

  if (is.null(stop_fun)) {
    if (is.infinite(stop_n)) {
      stop("this will run forever, provide a `stop_fun` or a finite `stop_n`")
    }
    stop_fun <- function(x) FALSE
  }

  if (requireNamespace("purrr", quietly = TRUE)) {
    f <- purrr::as_mapper(f)
    stop_fun <- purrr::as_mapper(stop_fun)
  }

  n <- 1
  while (n <= stop_n && !stop_fun(x)) {
    x <- f(x)
    if (accumulate) acc[[n + 1]] <- x # this is very inefficient, but what else?
    n <- n + 1
  }

  if (accumulate) acc else x

}
