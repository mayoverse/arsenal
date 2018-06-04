#' formulize
#'
#' A shortcut to generate one-, two-, or many-sided formulas from vectors of variable names.
#'
#' @param y,x,... Character vectors to be collapsed (by \code{"+"}) and put left-to-right in the formula.
#'   If \code{data} is supplied, these can also be numeric, denoting which column name to use. See examples.
#' @param data An R object with non-null column names.
#' @seealso \code{\link[stats]{reformulate}}
#' @author Ethan Heinzen
#' @examples
#' ## two-sided formula
#' formulize("y", c("x1", "x2", "x3"))
#'
#' ## one-sided formula
#' formulize(x = c("x1", "x2", "x3"))
#'
#' ## multi-sided formula
#' formulize("y", c("x1", "x2", "x3"), c("z1", "z2"), "w1")
#'
#' ## can use numerics for column names
#' data(mockstudy)
#' formulize(y = 1, x = 2:4, data = mockstudy)
#'
#' ## mix and match
#' formulize(1, c("x1", "x2", "x3"), data = mockstudy)
#'
#' ## get an interaction
#' formulize("y", c("x1*x2", "x3"))
#'
#' ## use in an lm
#' form <- formulize(2, 3:4, data = mockstudy)
#' summary(lm(form, data = mockstudy))
#'
#' @export

formulize <- function(y = "", x = "", ..., data = NULL)
{
  dots <- list(y = y, x = x, ...)
  if(!is.null(data))
  {
    if(is.null(colnames(data))) stop("colnames(data) is NULL")
    dots <- lapply(dots, function(elt, cn) if(is.numeric(elt)) lapply(cn[elt], as.name) else elt, cn = colnames(data))
  }
  is.ok <- function(x) is.character(x) || (is.list(x) && all(vapply(x, is.name, NA)))
  trash <- lapply(dots, function(elt) if(!is.ok(elt))
    stop("One or more argument isn't a character vector or list of names"))
  elts <- vapply(dots, paste0, character(1), collapse = " + ")
  stats::as.formula(paste0(elts, collapse = " ~ "), env = parent.frame())
}
