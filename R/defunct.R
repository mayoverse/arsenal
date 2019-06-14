
#' Defunct functions in \code{arsenal}
#'
#' Details about defunct functions in \code{arsenal}
#'
#' @param x,y See \code{\link{comparedf}}.
#' @param ... Other arguments.
#' @details
#' \code{comparison.control} was renamed to \code{\link{comparedf.control}} in version 3.0.0.
#'
#' \code{compare.data.frame} was renamed to \code{\link{comparedf}} in version 3.0.0.
#'
#' \code{length.tableby} was removed in version 2.0.0.
#'
#' \code{includeNA.character} and \code{includeNA.numeric} were removed in version 2.0.0 and
#'   replaced with a default method.
#'
#' \code{rangeTime} was removed in version 1.5.0.
#' @seealso \code{\link{arsenal-deprecated}}, \code{\link{comparedf}}
#' @name arsenal-defunct
NULL
#> NULL


#' @rdname arsenal-defunct
#' @export
comparison.control <- function(...)
{
  .Defunct("comparedf.control", package = "arsenal")
}

#' @rdname arsenal-defunct
#' @export
compare.data.frame <- function(x, y, ...)
{
  .Defunct("comparedf", package = "arsenal")
}
