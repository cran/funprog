#' Unique with auxiliary function
#'
#' Remove duplicate elements, given a transformation.
#'
#' @param x a vector or a list.
#' @param f a function to apply to each element of \code{x}. This function must
#'   produce comparable results.
#' @param first if several elements are identical after being transformed by
#'   \code{f}, keep the first. Otherwise, keep the last.
#'
#' @return An object of the same type as x. Only elements that are unique after
#'   being transformed by \code{f} are kept.
#' @export
#'
#' @examples
#' unique_by(-3:2, abs)
#' unique_by(-3:2, abs, first = FALSE)
#' unique_by(c(1, 2, 4, 5, 6), function(x) x %% 3)
#' unique_by(list(1:2, 2:3, 2:4), length)

unique_by <- function(x, f, first = TRUE) {

  if (requireNamespace("purrr", quietly = TRUE)) {
    f <- purrr::as_mapper(f)
  }

  x2 <- sapply(x, f)
  x[!duplicated(x2, fromLast = !first)]

}
