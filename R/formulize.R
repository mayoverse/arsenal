#' formulize
#'
#' A shortcut to generate one-, two-, or many-sided formulas from vectors of variable names.
#'
#' @param y,x,... Character vectors, names, or calls to be collapsed (by \code{"+"}) and put left-to-right in the formula.
#'   If \code{data} is supplied, these can also be numeric, denoting which column name to use. See examples.
#' @param data An R object with non-null column names.
#' @param collapse How should terms be collapsed? Default is addition.
#' @param collapse.y How should the y-terms be collapsed? Default is addition. Also accepts the special string "list",
#'   which combines them into a multiple-left-hand-side formula, for use in other functions.
#' @seealso \code{\link[stats]{reformulate}}
#' @author Ethan Heinzen
#' @examples
#' ## two-sided formula
#' f1 <- formulize("y", c("x1", "x2", "x3"))
#'
#' ## one-sided formula
#' f2 <- formulize(x = c("x1", "x2", "x3"))
#'
#' ## multi-sided formula
#' f3 <- formulize("y", c("x1", "x2", "x3"), c("z1", "z2"), "w1")
#'
#' ## can use numerics for column names
#' data(mockstudy)
#' f4 <- formulize(y = 1, x = 2:4, data = mockstudy)
#'
#' ## mix and match
#' f5 <- formulize(1, c("x1", "x2", "x3"), data = mockstudy)
#'
#' ## get an interaction
#' f6 <- formulize("y", c("x1*x2", "x3"))
#'
#' ## get only interactions
#' f7 <- formulize("y", c("x1", "x2", "x3"), collapse = "*")
#'
#' ## no intercept
#' f8 <- formulize("y", "x1 - 1")
#' f9 <- formulize("y", c("x1", "x2", "-1"))
#'
#' ## LHS as a list to use in arsenal functions
#' f10 <- formulize(c("y1", "y2", "y3"), c("x", "z"), collapse.y = "list")
#'
#' ## use in an lm
#' f11 <- formulize(2, 3:4, data = mockstudy)
#' summary(lm(f11, data = mockstudy))
#'
#' ## using non-syntactic names or calls (like reformulate example)
#' f12 <- formulize(as.name("+-"), c("`P/E`", "`% Growth`"))
#'
#' f <- Surv(ft, case) ~ a + b
#' f13 <- formulize(f[[2]], f[[3]])
#' @export

formulize <- function(y = "", x = "", ..., data = NULL, collapse = "+", collapse.y = collapse)
{
  dots <- list(y = y, x = x, ...)
  if(!is.null(data))
  {
    if(is.null(colnames(data))) stop("colnames(data) is NULL")
    dots <- lapply(dots, function(elt, cn) if(is.numeric(elt)) lapply(cn[elt], as.name) else elt, cn = colnames(data))
  }
  name.or.call <- function(elt) is.name(elt) || is.call(elt)
  dots <- lapply(dots, function(elt) if(name.or.call(elt)) list(elt) else elt)
  is.ok <- function(x) is.character(x) || (is.list(x) && all(vapply(x, name.or.call, NA)))
  trash <- lapply(dots, function(elt) if(!is.ok(elt))
    stop("One or more argument isn't a character vector, numeric vector, list of names, or list of calls"))

  dots[[1]] <- if(collapse.y == "list")
  {
    paste0("list(", paste0(dots[[1]], collapse = ", "), ")")
  } else paste0(dots[[1]], collapse = collapse.y)

  elts <- vapply(dots, paste0, character(1), collapse = collapse)
  stats::as.formula(paste0(elts, collapse = " ~ "), env = parent.frame())
}
