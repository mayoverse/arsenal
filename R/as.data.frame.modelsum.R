
get_the_estimate <- function(fitList, expon)
{
  # even if the model doesn't have an intercept, that's okay
  labs <- c("(Intercept)", fitList$label, fitList$adjlabels)
  names(labs) <- c("(Intercept)", fitList$xterms, fitList$adjterms)

  out <- data.frame(
    endpoint = rep(fitList$glance$endpoint, times = length(fitList$coeff$term)),
    term = fitList$coeff$term,
    label = labs[fitList$coeff$term],
    term.type = ifelse(fitList$coeff$term == "(Intercept)", "Intercept", ifelse(fitList$coeff$term %in% fitList$adjterms, "Adjuster", "Term")),
    stringsAsFactors = FALSE
  )
  if(expon && fitList$family %in% c("binomial", "quasibinomial", "survival", "poisson", "quasipoisson"))
  {
    type <- ifelse(fitList$family %in% c("binomial", "quasibinomial"), "OR", ifelse(fitList$family == "survival", "HR", "RR"))
    out[[type]] <- fitList$coeff[[type]]
    out[[paste0("CI.lower.", type)]] <- fitList$coeff[[paste0("CI.lower.", type)]]
    out[[paste0("CI.upper.", type)]] <- fitList$coeff[[paste0("CI.upper.", type)]]
  } else
  {
    out$estimate <- fitList$coeff$estimate
    out$std.error <- fitList$coeff$std.error
  }
  out$p.value = fitList$coeff$p.value
  out$adj.r.squared <- fitList$glance$adj.r.squared
  out
}

as_data_frame.modelsum <- function(x, ..., labelTranslations = NULL, show.intercept = NA, show.adjust = NA, exponentiate = FALSE)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations
  if(is.na(show.intercept)) show.intercept <- x$control$show.intercept
  if(is.na(show.adjust)) show.adjust <- x$control$show.adjust

  out <- do.call(rbind, Map(cbind, model = seq_along(x$fits), lapply(x$fits, get_the_estimate, expon = exponentiate))) # this step is almost magic
  out <- out[out$term.type %in% c("Term", if(show.intercept) "Intercept", if(show.adjust) "Adjuster"), , drop = FALSE]
  row.names(out) <- NULL
  out
}
