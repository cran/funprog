#' Transform a binary function with a unary function
#'
#' Execute the binary function f on the results of applying unary function g to
#' two arguments x and y.
#'
#' Formally, \code{\%on\%} is defined this way :
#' \code{function(f, g) function(x, y) f(g(x), g(y))}.
#'
#' f can be a function taking two arguments but also a variadic function (i.e.
#' whose first argument is \code{...}), which will be fed with exactly two
#' arguments.
#'
#' A typical usage of this function is in combination with function like
#' \code{\link{group_if}}.
#'
#' @param f a binary function.
#' @param g a unary function.
#'
#' @return A binary function. This function transforms 2 inputs (with g) and
#' combines the outputs (with f).
#'
#' @export
#' @rdname on
#' @aliases on
#'
#' @examples
#' h <- max %on% abs
#' h(-2, 1)

`%on%` <- function(f, g) {

  force(f)
  force(g)

  if (requireNamespace("purrr", quietly = TRUE)) {
    f <- purrr::as_mapper(f)
    g <- purrr::as_mapper(g)
  }

  function(x, y) f(g(x), g(y))

}
