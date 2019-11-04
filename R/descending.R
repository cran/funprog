#' Reverse a sorting function
#'
#' Transform a function (typically used in \code{\link{sort_by}}), so that its
#' ouput can be sorted in descending order.
#'
#' @param f a function to modify.
#'
#' @return A function returning a numeric vector which, if passed to
#' \code{\link{order}}, will be used to sort the original data.
#'
#' @examples
#' desc_abs <- descending(abs)
#'
#' x <- -2:1
#' order(abs(x))
#' order(desc_abs(x))
#' @export

descending <- function(f) {

  force(f) # to avoid lazy evaluation in a loop

  if (requireNamespace("purrr", quietly = TRUE)) {
    f <- purrr::as_mapper(f)
  }

  function(x) -xtfrm(f(x))

}
