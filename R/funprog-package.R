#' Implementation of pure functional programming languages functions
#'
#' The \strong{funprog} package implements in R some functions existing in other
#' pure functional programming languages.
#'
#' @section Main functions:
#' The package provides high-order functions, for example :
#' \itemize{
#'   \item{\code{\link{group_if}}, inspired by Haskell's \code{groupBy}}
#'   \item{\code{\link{sort_by}}, inspired by Haskell's \code{sortBy}}
#' }
#'
#' @section Helper functions:
#' Helper functions can be used in conjunction with the main functions :
#' \itemize{
#'   \item{\code{\%on\%} combines two functions into one and serves to create a
#'     predicate function to \code{group_if}}
#'   \item{\code{descending} is used to reverse the output of a sorting
#'     function used with \code{sort_by}}
#' }
#'
#' @section \code{purrr} syntax:
#' If the \href{https://purrr.tidyverse.org}{purrr} package is installed, you
#' can use its special syntax to create very compact anonymous functions, for
#' example \code{~ abs(.x - .y) > 1} instead of \code{function(x, y) abs(x - y)
#' > 1}.
#'
#' @docType package
#' @name funprog-package

NULL
