#' Family functions for modelsum
#'
#' A set of family functions for \code{\link{modelsum}}.
#'
#' @param method See \code{MASS::\link[MASS]{polr}}.
#' @return A list, in particular with element \code{family}.
#' @seealso \code{\link[stats]{family}}, \code{\link[survival]{coxph}}, \code{\link[MASS]{polr}}
#' @name modelsum.family
NULL
#> NULL

#' @rdname modelsum.family
#' @export
survival <- function() list(family="survival")

#' @rdname modelsum.family
#' @export
ordinal <- function(method = c("logistic", "probit", "loglog", "cloglog", "cauchit"))
{
  list(family = "ordinal", method = match.arg(method))
}
