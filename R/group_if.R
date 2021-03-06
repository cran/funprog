#' Group vector values
#'
#' Split a vector or a list into groups, given a predicate function.
#'
#' \code{predicate} will be applied to 2 adjacent elements. If it evaluates to
#' \code{TRUE}, those elements belong to the same group, otherwise they belong
#' to different groups.
#'
#' Grouping on equality is the most natural approach, therefore \code{group_eq}
#' is a convenient shortcut defined as \itemize{
#'   \item{\code{group_if(x, predicate = `==`)} for an atomic vector;}
#'   \item{\code{group_if(x, predicate = identical)} for a list.}
#' }
#'
#' \code{group_if} (resp. \code{group_eq}) is inspired by \code{groupBy} (resp.
#' \code{group}) in Haskell.
#' \emph{Note that \code{group_if} behaves a little differently : while in
#' Haskell, the comparison is made with the first element in the group, in this
#' R-version the comparison is made with the adjacent element.}
#'
#' The operator \link{\%on\%} may be helpful to create a predicate with readable
#' syntax.
#'
#' @param x a vector or a list to split into groups.
#' @param predicate a binary function returning a boolean value.
#' @param na.rm if x is atomic, delete missing values before grouping.
#'
#' @return A list where each element is a group (flattening this list should
#'   give back the same values in the same order). Element names are kept.
#'
#' @examples
#' x1 <- c(3, 4, 2, 2, 1, 1, 1, 3)
#' group_eq(x1)
#' group_if(x1, `<=`)
#' group_if(x1, function(x, y) abs(x - y) > 1)
#'
#' x2 <- c(3, 4, 2, -2, -1, 1, 1, 3)
#' group_if(x2, `==` %on% abs)
#'
#' x3 <- list(1:3, 1:3, 3:5, 1, 2)
#' group_if(x3, `==` %on% length)
#' @export

group_if <- function(x, predicate, na.rm = FALSE) {

  if (requireNamespace("purrr", quietly = TRUE)) {
    predicate <- purrr::as_mapper(predicate)
  }

  if (!length(x)) return(list())

  if (is.atomic(x)) {
    # check NA
    if (anyNA(x)) {
      if (na.rm) {
        x <- x[!is.na(x)]
      } else {
        stop("impossible, missing values")
      }
    }
  }

  prev_same_grp <- mapply(predicate, x[-length(x)], x[-1])
  grps <- cumsum(c(FALSE, !prev_same_grp))
  res <- unname(split(x, grps))

  # reapply names
  reapply_names(res, names(x))

}

#' @rdname group_if
#' @export

group_eq <- function(x, na.rm = FALSE) {

  if (is.atomic(x)) {
    group_if(x, predicate = `==`, na.rm = na.rm)
  } else {
    group_if(x, predicate = identical)
  }

}


# Helper function ---------------------------------------------------------

reapply_names <- function(x, nm) {

  if (is.null(nm)) return(x)

  csl <- cumsum(lengths(x))

  mapply(
    function(v, start, end) {
      nm_v <- nm[seq(start, end)]
      if (all(is.na(nm_v)) | all(nm_v == "")) nm_v <- NULL
      stats::setNames(v, nm_v)
    },
    x,
    c(1, csl[-length(csl)] + 1),
    csl,
    SIMPLIFY = FALSE
  )

}
