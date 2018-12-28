#' Adjust P-values for Multiple Comparisons
#'
#' @param p An object.
#' @inheritParams stats::p.adjust
#' @param suffix A suffix to add to the footnotes indicating that the tests were adjusted.
#' @param ... Other arguments.
#' @seealso \code{\link[stats]{p.adjust}}, \code{\link{modpval.tableby}}, \code{\link{tests.tableby}}
#' @name padjust
NULL
#> NULL

#' @rdname padjust
#' @export
padjust <- function(p, method, n, ...) UseMethod("padjust")

#' @rdname padjust
#' @export
padjust.default <- function(p, method, n, ...)
{
  Call <- match.call()
  indx <- match(c("p", "method", "n"), names(Call), nomatch = 0)
  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::p.adjust)

  eval(temp.call, parent.frame())
}

#' @rdname padjust
#' @export
padjust.tableby <- function(p, method, n, suffix = " (adjusted for multiple comparisons)", ...)
{
  Call <- match.call()
  if(p$hasStrata || length(p$tables) > 1) stop("Can't adjust p-values on tables with strata or multiple by-variables.")
  if(!p$control$test) stop("Can't adjust p-values when no tests were run")
  indx <- match(c("p", "method", "n"), names(Call), nomatch = 0)
  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::p.adjust)

  pvals <- tests(p)
  temp.call$p <- pvals$p.value
  pvals$p.value <- eval(temp.call, parent.frame())
  pvals$Method <- paste0(pvals$Method, suffix)
  modpval.tableby(p, pvals)
}

#' @rdname padjust
#' @export
padjust.summary.tableby <- function(p, method, n, suffix = " (adjusted for multiple comparisons)", ...)
{
  Call <- match.call()
  if(p$hasStrata || length(p$object) > 1) stop("Can't adjust p-values on tables with strata or multiple by-variables")
  if(!p$control$test)
  {
    warning("No tests run on tableby object")
    return(p)
  }
  indx <- match(c("p", "method", "n"), names(Call), nomatch = 0)
  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::p.adjust)
  pvals <- unique(p$object[[1]][c("variable", "p.value")]) # find unique variable-pval combos
  temp.call$p <- pvals$p.value
  pvals$p.value <- eval(temp.call, parent.frame())
  p$object[[1]][["p.value"]] <- pvals$p.value[match(p$object[[1]][["variable"]], pvals$variable)] # "merge" them back in
  p$object[[1]][["test"]] <- paste0(p$object[[1]][["test"]], suffix)
  p
}
