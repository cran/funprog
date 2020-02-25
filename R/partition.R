#' Partition a vector in two
#'
#' Split a vector or a list in 2 groups, given a predicate function.
#'
#' @param x vector or list to partition.
#' @param predicate a function returning a boolean value, to apply to each
#'   element of x.
#'
#' @return A list of two elements. The first element contains elements of x
#'   satisfying the predicate, the second the rest of x. Missing values will be
#'   discarded.
#' @export
#'
#' @examples
#' partition(c(2, 1, 3, 4, 1, 5), function(x) x < 3)
#' partition(list(1:3, NA, c(1, NA, 3)), anyNA)

partition <- function(x, predicate) {

  if (requireNamespace("purrr", quietly = TRUE)) {
    predicate <- purrr::as_mapper(predicate)
  }

  p <- vapply(x, predicate, logical(1))
  list(
    x[which(p)],
    x[which(!p)]
  )
  # use of which() to handle NA

}
