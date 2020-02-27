
get_ms_strata_part <- function(msList, sValue, xList, ...)
{
  Map(get_ms_part, msList, c(0, cumsum(lengths(msList)[-1])), xList, MoreArgs = list(sValue = sValue, ...))
}

get_ms_part <- function(msList, modelnum, xList, yList, aList, sList, sValue, fam, cntrl)
{
  get_labs <- function(x) stats::setNames(x$label, x$term)
  # even if the model doesn't have an intercept, that's okay
  labs <- c("(Intercept)" = "(Intercept)", get_labs(xList), adj <- unlist(unname(lapply(aList, get_labs))))
  statFields <- switch(
    fam,
    quasibinomial = cntrl$binomial.stats, binomial = cntrl$binomial.stats,
    quasipoisson = cntrl$poisson.stats, poisson = cntrl$poisson.stats,
    negbin = cntrl$negbin.stats, clog = cntrl$clog.stats, survival = cntrl$survival.stats,
    ordinal = cntrl$ordinal.stats, gaussian = cntrl$gaussian.stats,
  )

  OUT <- NULL
  for(adj.i in seq_along(msList))
  {
    msLst <- msList[[adj.i]]
    trms <- msLst$coeff$term
    out <- data.frame(
      y.term = yList$term,
      y.label = yList$label,
      strata.term = if(!sList$hasStrata) "" else paste0("(", sList$term, ") == ", sValue),
      strata.value = if(!sList$hasStrata) "" else sValue,
      adjustment = names(msList)[adj.i],
      model = modelnum + adj.i,
      term = trms,
      label = ifelse(trms %in% names(labs), labs[trms], trms),
      term.type = ifelse(trms %in% names(adj), "Adjuster", ifelse(trms %in% xList$term, "Term", "Intercept")),
      stringsAsFactors = FALSE
    )
    if(!sList$hasStrata) out$strata.value <- NULL else names(out)[4] <- sList$label
    if(any(names(msLst$coeff) %in% statFields)) out <- cbind(out, msLst$coeff[intersect(statFields, names(msLst$coeff))])
    if(any(names(msLst$glance) %in% statFields)) out <- cbind(out, msLst$glance[intersect(statFields, names(msLst$glance))])
    OUT <- rbind_chr(OUT, out)
  }
  OUT
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
as.data.frame.modelsum <- function(x, ..., labelTranslations = NULL, list.ok = FALSE)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations

  control <- c(list(...), x$control)
  control <- do.call("modelsum.control", control[!duplicated(names(control))])

  out <- lapply(x$tables, as_data_frame_modelsum, control = control)

  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.modelsum is returning a list of data.frames")
  }

  set_attr(out, "control", control)
}

as_data_frame_modelsum <- function(lhsList, control)
{
  stopifnot(length(lhsList$tables) == length(lhsList$strata$values))
  tabs <- Map(get_ms_strata_part, msList = lhsList$tables, sValue = lhsList$strata$values,
              MoreArgs = list(yList = lhsList$y, sList = lhsList$strata, xList = lhsList$x, aList = lhsList$adjust,
                              fam = lhsList$family, cntrl = control))
  out <- do.call(rbind_chr, unlist(tabs, recursive = FALSE, use.names = FALSE))

  out <- out[out$term.type %in% c("Term", if(control$show.intercept) "Intercept", if(control$show.adjust) "Adjuster"), , drop = FALSE]
  row.names(out) <- NULL

  # Get rid of Nmiss if none missing
  if("Nmiss" %in% names(out) && all(out$Nmiss == 0)) out$Nmiss <- NULL
  set_attr(out, "ylabel", lhsList$y$label)
}
