#' Sort with auxiliary function
#'
#' Sort a vector or a list, given one or more auxiliary functions.
#'
#' The output of the first function will be used as first key for sorting,
#' the output of the second function as second key, and so on...
#' Therefore, these outputs should be sortable (i.e. atomic vectors).
#'
#' \code{sort_by} is inspired by \code{sortBy} in Haskell.
#'
#' @param x vector or list to sort.
#' @param ... one or several functions to apply to \code{x}. Use
#'   \code{\link{descending}} for reversed order.
#' @param method the method for ties (see \code{\link{order}}).
#'
#' @return A vector or list containing rearranged elements of \code{x}.
#'
#' @seealso \code{\link{order}} which is used for rearranging elements.
#'
#' @examples
#' sort_by(-3:2, abs)
#' sort_by(-3:2, abs, function(x) -x)
#' sort_by(list(5:7, 0, 1:4), length)
#' sort_by(list(1:2, 3:4, 5), length, descending(sum))
#' @export

sort_by <- function(x,
                    ...,
                    method = c("auto", "shell", "radix")) {

  if (utils::packageVersion("base") < "3.4.0") {
    if (method[1] == "auto") method <- "shell" # auto didn't exist before 3.4.0
  } else {
    method <- match.arg(method)
  }

  funs <- list(...)
  if (requireNamespace("purrr", quietly = TRUE)) {
    funs <- lapply(funs, purrr::as_mapper)
  }

  sort_crit <- lapply(funs, function(f) sapply(x, f))

  all_atomic <- all(vapply(sort_crit, is.atomic, logical(1L)))
  all_atomic || stop("all outputs of the functions must be atomic")

  ord <-
    do.call(
      order,
      c(sort_crit, method = method)
    )

  x[ord]

}
