
#' Make a column for "select all" input
#'
#' @param ... Named arguments of the same length. These should be logical, numeric (0/1) or a factor with two levels.
#' @param i,j,drop Arguments to `[.matrix`
#' @param x An object of class "selectall"
#' @examples
#' d <- data.frame(grp = rep(c("A", "B"), each = 5))
#' d$s <- selectall(
#'   `Option 1` = c(rep(1, 4), rep(0, 6)),
#'   `Option 2` = c(0, 1, 0, 0, 0, 1, 1, 1, 0, 0),
#'   `Option 3` = 1,
#'   `Option 4` = 0
#' )
#' summary(tableby(grp ~ s, data = d), text = TRUE)
#' @seealso \code{\link{tableby}}, \code{\link{paired}}
#' @name selectall
NULL
#> NULL

#' @rdname selectall
#' @export
selectall <- function(...)
{
  Call <- match.call()
  args <- lapply(list(...), function(x) if(is.factor(x)) as.numeric(x)-1 else if(is.numeric(x)) x else if(is.logical(x)) +x else as.numeric(factor(x))-1)
  out <- do.call(cbind, args)

  if(any(out %nin% c(0, 1, NA))) stop("Some elements may not have two levels")
  as.selectall(out)
}

#' @rdname selectall
#' @export
as.selectall <- function(x)
{
  structure(x, class = c("selectall", class(x)[class(x) != "selectall"]))
}

#' @rdname selectall
#' @export
as.matrix.selectall <- function(x, ...)
{
  class(x) <- "matrix"
  x
}

#' @rdname selectall
#' @export
`[.selectall` <- function(x, i, j, drop = FALSE)
{
  as.selectall(as.matrix(x)[i, j, drop = FALSE])
}

#' @rdname selectall
#' @export
is.na.selectall <- function(x)
{
  rowSums(is.na(as.matrix(x))) > 0
}

#' @rdname selectall
#' @export
is.selectall <- function(x) inherits(x, "selectall")
