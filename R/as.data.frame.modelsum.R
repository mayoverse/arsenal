
get_the_estimate <- function(fitList, cntrl)
{
  # even if the model doesn't have an intercept, that's okay
  labs <- c("(Intercept)", fitList$label, fitList$adjlabels)
  names(labs) <- c("(Intercept)", fitList$xterms, fitList$adjterms)
  trms <- fitList$coeff$term

  out <- data.frame(
    term = trms,
    label = ifelse(trms %in% names(labs), labs[trms], trms),
    term.type = ifelse(trms %in% fitList$adjterms, "Adjuster",
                       ifelse(trms %in% fitList$xterms, "Term", "Intercept")),
    stringsAsFactors = FALSE
  )

  statFields <- switch(fitList$family,
                       quasibinomial = cntrl$binomial.stats, binomial = cntrl$binomial.stats,
                       quasipoisson = cntrl$poisson.stats, poisson = cntrl$poisson.stats,
                       negbin = cntrl$negbin.stats,
                       survival = cntrl$survival.stats, ordinal = cntrl$ordinal.stats,
                       cntrl$gaussian.stats)

  if(any(names(fitList$coeff) %in% statFields)) out <- cbind(out, fitList$coeff[, intersect(statFields, names(fitList$coeff)), drop = FALSE])
  if(any(names(fitList$glance) %in% statFields)) out <- cbind(out, fitList$glance[intersect(statFields, names(fitList$glance))])
  out
}

#' as.data.frame.modelsum
#'
#' Coerce a \code{\link{modelsum}} object to a \code{data.frame}.
#'
#' @param x A \code{\link{modelsum}} object.
#' @param ... Arguments to pass to \code{\link{modelsum.control}}.
#' @inheritParams summary.modelsum
#' @seealso \code{\link{modelsum}}, \code{\link{summary.modelsum}}
#' @return A \code{data.frame}.
#' @author Ethan Heinzen, based on code originally by Greg Dougherty
#' @export
as.data.frame.modelsum <- function(x, ..., labelTranslations = NULL)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations

  control <- c(list(...), x$control)
  control <- do.call("modelsum.control", control[!duplicated(names(control))])

  out <- do.call(rbind, c(Map(cbind, model = seq_along(x$fits), lapply(x$fits, get_the_estimate, cntrl = control)),
                          make.row.names = FALSE)) # this step is almost magic
  out <- out[out$term.type %in% c("Term", if(control$show.intercept) "Intercept", if(control$show.adjust) "Adjuster"), , drop = FALSE]
  row.names(out) <- NULL # in case we dropped some terms

  idx <- vapply(out, is.factor, NA)
  if(any(idx)) out[idx] <- lapply(out[idx], as.character) ## this is for R 3.2.3, whose rbind() doesn't have 'stringsAsFactors='

  # Get rid of Nmiss if none missing
  if("Nmiss" %in% colnames(out) && all(out$Nmiss == 0)) out$Nmiss <- NULL

  set_attr(out, "control", control)
}
