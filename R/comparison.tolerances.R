
#' Comparison tolerances
#'
#' Internal functions defining tolerances for the \code{\link{comparison.control}} function.
#'   To create your own tolerance definitions, see the vignette.
#'
#' @param x,y vectors of the appropriate lengths and types.
#' @param tol A numeric tolerance
#' @return A logical vector of length equal to that of \code{x} and \code{y}.
#' @author Ethan Heinzen
#' @seealso \code{\link{comparison.control}}, \code{\link{compare.data.frame}}
#' @name comparison.tolerances
NULL
#> NULL

#' @rdname comparison.tolerances
#' @export
tol.num.absolute <- function(x, y, tol)
{
  abs(x - y) > tol
}

#' @rdname comparison.tolerances
#' @export
tol.num.percent <- tol.num.pct <- function(x, y, tol)
{
  abs((x - y)/x) > tol
}

#' @rdname comparison.tolerances
#' @export
tol.factor.none <- function(x, y)
{
  (as.character(x) != as.character(y)) | (as.numeric(x) != as.numeric(y))
}

#' @rdname comparison.tolerances
#' @export
tol.factor.levels <- function(x, y)
{
  as.numeric(x) != as.numeric(y)
}

#' @rdname comparison.tolerances
#' @export
tol.factor.labels <- function(x, y)
{
  as.character(x) != as.character(y)
}

#' @rdname comparison.tolerances
#' @export
tol.char.both <- function(x, y)
{
  tolower(trimws(x)) != tolower(trimws(y))
}

#' @rdname comparison.tolerances
#' @export
tol.char.case <- function(x, y)
{
  tolower(x) != tolower(y)
}

#' @rdname comparison.tolerances
#' @export
tol.char.trim <- function(x, y)
{
  trimws(x) != trimws(y)
}

#' @rdname comparison.tolerances
#' @export
tol.char.none <- function(x, y)
{
  x != y
}
