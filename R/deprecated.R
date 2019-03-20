
#' Deprecated functions in \code{arsenal}
#'
#' Details about deprecated functions in \code{arsenal}
#'
#' @param ... Arguments passed to functions that aren't deprecated.
#' @param x,y See \code{\link{comparedf}}.
#' @details
#' \code{comparison.control} was renamed to \code{\link{comparedf.control}}.
#'
#' \code{compare.data.frame} was renamed to \code{\link{comparedf}}.
#' @seealso \code{\link{arsenal-defunct}}
#' @name arsenal-deprecated
NULL
#> NULL

#' @rdname arsenal-deprecated
#' @export
comparison.control <- function(...)
{
  Call <- match.call()
  .Deprecated("comparedf.control", package = "arsenal")
  Call[[1]] <- quote(comparedf.control)
  eval(Call, parent.frame())
}

#' @rdname arsenal-deprecated
#' @export
compare.data.frame <- function(x, y, ...)
{
  Call <- match.call()
  .Deprecated("comparedf", package = "arsenal")
  Call[[1]] <- quote(comparedf)
  eval(Call, parent.frame())
}
