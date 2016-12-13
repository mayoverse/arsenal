## Purpose: control parameters for modelsum function
## Authors: P Votruba, Jason Sinnwell, Beth Atkinson
## Created: 9/3/2015

#' Control settings for \code{modelsum} function
#' 
#' Control test and summary settings for \code{\link{modelsum}} function.
#' 
#' @param digits Numeric, denoting the number of significant digits for beta coefficient and standard error.
#' @param digits.test Numeric, denoting the number of significant digits for p-values.
#' @param nsmall Numeric, denoting the number of digits after the decimal point for beta coefficient and standard error.
#' @param nsmall.ratio Numeric, denoting the number of digits after the decimal point for ratios, e.g. OR, RR, HR.
#' @param show.adjust Logical, denoting whether to show adjustment terms.
#' @param show.intercept Logical, denoting whether to show intercept terms.
#' @param conf.level Numeric, giving the confidence level.
#' @param binomial.stats,survival.stats,gaussian.stats,poisson.stats
#'   Character vectors denoting which stats to show for the various model types.
#' @param ... Other arguments (not in use at this time).
#' @return A list with settings to be used within the \code{modelsum} function.
#' @seealso \code{\link{modelsum}}, \code{\link{summary.modelsum}}
#' @export
modelsum.control <- function(digits=3, nsmall=NULL, nsmall.ratio=2, digits.test=3,
            show.adjust=TRUE, show.intercept=TRUE, conf.level=0.95,
            binomial.stats=c("OR","CI.lower.OR","CI.upper.OR","p.value", "concordance","Nmiss"),
            gaussian.stats=c("estimate","std.error","p.value","adj.r.squared","Nmiss"),
            poisson.stats=c("RR","CI.lower.RR", "CI.upper.RR","p.value","concordance","Nmiss"),
            survival.stats=c("HR","CI.lower.HR","CI.upper.HR","p.value","concordance","Nmiss"),
			...) {
		
  ## validate digits
  if(is.null(digits)) {
    digits <- 3
  }
 
  if(digits < 1) {
	  warning("digits must be positive integer. Set to default. \n")
	  digits <- 3
  }
  if(conf.level <=0 | conf.level >= 1) {
    warning("conf.level must be between (0,1). Setting to default.\n")
    conf.level <- .95
  }

  ##########################
  ## Binomial stats:
  ##########################
  ##Other coefficient columns:
  ##CI.estimate, N, Nmiss2, depvar (show name of dependent variable), estimate, se, zstat
  ##Other  model fits:  “logLik”,”AIC”,”BIC”
  binomial.stats.valid <- c(c("Nmiss","OR","CI.lower.OR","CI.upper.OR","p.value","concordance"), # default
                 c("estimate","CI.OR","CI.estimate","CI.lower.estimate","CI.upper.estimate","N","Nmiss2","endpoint","std.error","statistic"),
                 c("logLik","AIC","BIC","null.deviance","deviance","df.residual","df.null"))

  if(any(!(binomial.stats %in% binomial.stats.valid))) {
    stop(paste0("Invalid binomial stats: ",
           paste(binomial.stats[!(binomial.stats %in% binomial.stats.valid)],collapse=","), "\n"))   
  }
  ## let CI.OR decode to CI.lower.OR and CI.upper.OR
  if(any(grepl("CI.OR", binomial.stats))) {
    binomial.stats <- unique(c("CI.lower.OR","CI.upper.OR", binomial.stats[-grep("CI.OR",binomial.stats)]))
  }
  if(any(grepl("CI.estimate", binomial.stats))) {
    binomial.stats <- unique(c("CI.lower.estimate","CI.upper.estimate", binomial.stats[-grep("CI.estimate",binomial.stats)]))
  }
  
  ##########################
  ## Gaussian stats:
  ##########################
  ##Other coefficient columns: CI.estimate, N, Nmiss2, “t.stat”, standard.estimate, endpoint
  ##Other model fits: “r.squared”, “AIC”, “BIC”,“logLik”
  gaussian.stats.valid <- c(c("Nmiss","estimate","std.error","p.value", "adj.r.squared"), #default
              c("CI.estimate","CI.lower.estimate","CI.upper.estimate", "N", "Nmiss2", "statistic", "standard.estimate", "endpoint"),
              c("r.squared", "AIC", "BIC","logLik","statistic.F","p.value.F"))

  if(any(!(gaussian.stats %in% gaussian.stats.valid))) {
    stop(paste0("Invalid gaussian stats: ",
           paste(gaussian.stats[!(gaussian.stats %in% gaussian.stats.valid)],collapse=","), "\n"))
  }
  if(any(grepl("CI.estimate", gaussian.stats))) {
    gaussian.stats <- unique(c("CI.lower.estimate","CI.upper.estimate", gaussian.stats[-grep("CI.estimate",gaussian.stats)]))
  }

  
##########################
  ## Poisson stats:
  ##########################
  ##(quasi)/poisson.stats=c("Nmiss","RR","CI.RR", "p.value","concordance"),
  ##Other coeff columns: CI.estimate, CI.RR  (ci for relrisk),N,Nmiss2, std.error, estimate, z.stat, endpoint
  ##Other model fits: “AIC”, “BIC”, “logLik”, dispersion
  ##  dispersion = deviance/df.residual
  poisson.stats.valid <- c(c("RR","CI.lower.RR","CI.upper.RR", "p.value","concordance", "Nmiss"), # default
                   c("CI.RR","CI.estimate","CI.lower.estimate","CI.upper.estimate", "CI.RR", "Nmiss2", "std.error", "estimate", "statistic","endpoint"),
                   c("AIC", "BIC","logLik","dispersion","null.deviance","deviance","df.residual","df.null"))
  
  if(any(!(poisson.stats %in% poisson.stats.valid))) {
      stop(paste0("Invalid poisson stats: ",
           paste(poisson.stats[!(poisson.stats %in% poisson.stats.valid)],collapse=","), "\n"))
  }
   ## let CI.RR decode to CI.lower.RR and CI.upper.RR
  if(any(grepl("CI.RR", poisson.stats))) {
    poisson.stats <- unique(c("CI.lower.RR","CI.upper.RR", poisson.stats[-grep("CI.RR",poisson.stats)]))
  }
  if(any(grepl("CI.estimate", poisson.stats))) {
    poisson.stats <- unique(c("CI.lower.estimate","CI.upper.estimate", poisson.stats[-grep("CI.estimate",poisson.stats)]))
  }
  ##########################
  ## Survival stats:
  ##########################
  ##surv.stats=c(“Nmiss”,"HR",”CI.HR”,"p.value","concorance")
  ##Other possible coefficient table columns: CI.estimate,N,Nmiss2,”estimate”,”se”,”endpoint”,”Nevents”,z.stat
  ##Other possible model fits: r.squared, logLik, AIC, BIC
  surv.stats.valid <- c(c("HR","CI.lower.HR","CI.upper.HR","p.value","concordance","Nmiss"), # default
            c("CI.HR","CI.estimate","CI.lower.estimate","CI.upper.estimate","N","Nmiss2","estimate","std.error","endpoint","Nevents","statistic"),
            c("r.squared", "logLik", "AIC", "BIC","statistic.sc","p.value.sc","p.value.log","p.value.wald","N","std.error.concordance"))
  if(any(!(survival.stats %in% surv.stats.valid))) {
    stop(paste0("Invalid survival stats: ",
           paste(survival.stats[!(survival.stats %in% surv.stats.valid)],collapse=","), "\n"))
  }

  ## let CI.HR decode to CI.lower.HR and CI.upper.HR
  if(any(grepl("CI.HR", survival.stats))) {
    survival.stats <- unique(c("CI.lower.HR","CI.upper.HR", survival.stats[-grep("CI.HR",survival.stats)]))
  }
  if(any(grepl("CI.estimate", survival.stats))) {
    survival.stats <- unique(c("CI.lower.estimate","CI.upper.estimate", survival.stats[-grep("CI.estimate",survival.stats)]))
  }
  return(list(digits=digits, digits.test=digits.test, nsmall=nsmall, nsmall.ratio=nsmall.ratio,
              show.adjust=show.adjust, show.intercept=show.intercept, conf.level=conf.level,
              binomial.stats=binomial.stats, gaussian.stats=gaussian.stats,
              poisson.stats=poisson.stats, survival.stats=survival.stats))
  
}
