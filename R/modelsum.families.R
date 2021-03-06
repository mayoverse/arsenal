#' Family functions for modelsum
#'
#' A set of family functions for \code{\link{modelsum}}.
#'
#' @param method See \code{MASS::\link[MASS]{polr}}.
#' @param link See \code{MASS::\link[MASS]{glm.nb}}.
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
  list(family = "ordinal", method = if(is.function(method)) method else match.arg(method))
}

#' @rdname modelsum.family
#' @export
negbin <- function(link = c("log", "identity", "sqrt"))
{
  list(family = "negbin", method = if(is.function(link)) link else match.arg(link))
}

#' @rdname modelsum.family
#' @export
clog <- function()
{
  list(family = "clog")
}


#' @rdname modelsum.family
#' @export
relrisk <- function(link = "log")
{
  list(family = "relrisk", link = link)
}

