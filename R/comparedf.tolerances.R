
#' Comparison tolerances
#'
#' Internal functions defining tolerances for the \code{\link{comparison.control}} function.
#'   To create your own tolerance definitions, see the vignette.
#'
#' @param x,y vectors of the appropriate lengths and types.
#' @param tol A numeric tolerance
#' @param idx A logical vector of appropriate length.
#' @return A logical vector of length equal to that of \code{x} and \code{y}, where \code{TRUE} denotes a
#'   difference between \code{x} and \code{y}, and \code{FALSE} denotes no difference between \code{x} and \code{y}.
#' @details
#' \code{tol.NA} takes as differences between two vectors any elements which are NA in one but not the other,
#'   or which are non-NA in both and \code{TRUE} in \code{idx}. It is useful for handling NAs in custom tolerance functions.
#' @author Ethan Heinzen
#' @seealso \code{\link{comparison.control}}, \code{\link{compare.data.frame}}
#' @name comparison.tolerances
NULL
#> NULL


#' @rdname comparison.tolerances
#' @export
tol.NA <- function(x, y, idx)
{
  (is.na(x) & !is.na(y)) | (is.na(y) & !is.na(x)) | (!is.na(x) & !is.na(y) & idx)
}

#' @rdname comparison.tolerances
#' @export
tol.num.absolute <- function(x, y, tol)
{
  tol.NA(x, y, abs(x - y) > tol)
}

#' @rdname comparison.tolerances
#' @export
tol.num.percent <- tol.num.pct <- function(x, y, tol)
{
  tol.NA(x, y, abs((x - y)/x) > tol)
}

#' @rdname comparison.tolerances
#' @export
tol.num.pct <- tol.num.percent

#' @rdname comparison.tolerances
#' @export
tol.factor.none <- function(x, y)
{
  tol.NA(x, y, (as.character(x) != as.character(y)) | (as.numeric(x) != as.numeric(y)))
}

#' @rdname comparison.tolerances
#' @export
tol.factor.levels <- function(x, y)
{
  tol.NA(x, y, as.numeric(x) != as.numeric(y))
}

#' @rdname comparison.tolerances
#' @export
tol.factor.labels <- function(x, y)
{
  tol.NA(x, y, as.character(x) != as.character(y))
}

#' @rdname comparison.tolerances
#' @export
tol.char.both <- function(x, y)
{
  tol.NA(x, y, tolower(trimws(x)) != tolower(trimws(y)))
}

#' @rdname comparison.tolerances
#' @export
tol.char.case <- function(x, y)
{
  tol.NA(x, y, tolower(x) != tolower(y))
}

#' @rdname comparison.tolerances
#' @export
tol.char.trim <- function(x, y)
{
  tol.NA(x, y, trimws(x) != trimws(y))
}

#' @rdname comparison.tolerances
#' @export
tol.char.none <- function(x, y)
{
  tol.NA(x, y, x != y)
}

#' @rdname comparison.tolerances
#' @export
tol.date.absolute <- tol.num.absolute

#' @rdname comparison.tolerances
#' @export
tol.logical.none <- tol.char.none

#' @rdname comparison.tolerances
#' @export
tol.other.none <- function(x, y)
{
  unlist(Map(Negate(identical), x, y))
}
