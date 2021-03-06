% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_by.R
\name{sort_by}
\alias{sort_by}
\title{Sort with auxiliary function}
\usage{
sort_by(x, ..., method = c("auto", "shell", "radix"))
}
\arguments{
\item{x}{vector or list to sort.}

\item{...}{one or several functions to apply to \code{x}. Use
\code{\link{descending}} for reversed order.}

\item{method}{the method for ties (see \code{\link{order}}).}
}
\value{
A vector or list containing rearranged elements of \code{x}.
}
\description{
Sort a vector or a list, given one or more auxiliary functions.
}
\details{
The output of the first function will be used as first key for sorting,
the output of the second function as second key, and so on...
Therefore, these outputs should be sortable (i.e. atomic vectors).

\code{sort_by} is inspired by \code{sortBy} in Haskell.
}
\examples{
sort_by(-3:2, abs)
sort_by(-3:2, abs, function(x) -x)
sort_by(list(5:7, 0, 1:4), length)
sort_by(list(1:2, 3:4, 5), length, descending(sum))
}
\seealso{
\code{\link{order}} which is used for rearranging elements.
}
