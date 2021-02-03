## Purpose: internal functions (and methods) for tableby function
## Authors: Jason Sinnwell, Beth Atkinson
## Created: 9/4/2015

## Helper functions for modelsum:  merge, subset, and labels (work like names)

#' Helper functions for modelsum
#'
#' A set of helper functions for \code{\link{modelsum}}.
#'
#' @param object A \code{data.frame} resulting from evaluating a \code{modelsum} formula.
#' @param ... Other arguments, or a vector of indices for extracting.
#' @param x A \code{modelsum} object.
#' @return \code{na.modelsum} returns a subsetted version of \code{object} (with attributes).
#' @seealso \code{\link{arsenal_table}}
#' @name modelsum.internal
NULL
#> NULL

join_formula <- function(x, y)
{
  x <- stats::formula(x)
  if(is.null(y)) return(x)
  y <- stats::formula(y)
  stopifnot(length(y) == 2)
  if(length(x) == 2)
  {
    x[[2]] <- call("+", x[[2]], y[[2]])
  } else
  {
    stopifnot(length(x) == 3)
    x[[3]] <- call("+", x[[3]], y[[2]])
  }
  x
}

#' @rdname modelsum.internal
#' @export
is.modelsum <- function(x) inherits(x, "modelsum")

#' @rdname modelsum.internal
#' @export
is.summary.modelsum <- function(x) inherits(x, "summary.modelsum")

#' @rdname modelsum.internal
#' @export
na.modelsum <- na_lhs_strata

##standardized beta function (for gaussian stat)
lm.beta  <- function (MOD) {
    b <- stats::coef(MOD)[-1]
    sx <- rep(NA,length(b))
    b.idx <- 1
    for(k in 2:ncol(MOD$model)) {
      ## skip factors and char variables,
      ## psplines consider doing sx, but need a second for loop for the ncol of those
      if(any(class(MOD$model[,k]) %in% c("character","factor", "pspline"))) {
        b.idx <- b.idx + ifelse(is.null(ncol(MOD$model[,k])), length(unique(MOD$model[,k]))-1, ncol(MOD$model[,k]))
        ## skip as many elements of beta as there are N.levels-1 of categorical variables
      } else {
        sx[b.idx] <- stats::sd(as.double(MOD$model[,k]),na.rm=TRUE)
        b.idx <- b.idx + 1
      }
    }
    sy <- stats::sd(as.double(MOD$model[,1]),na.rm=TRUE)
    beta <- c(NA,round(b * sx/sy,3))
    return(beta)
}

make_ms_term_labels <- function(mf, trms)
{
  factors <- attr(trms, "factors")
  mm <- stats::model.matrix(trms, mf)
  assign <- attr(mm, "assign")
  mm <- mm[, assign > 0, drop = FALSE]
  assign <- assign[assign > 0]
  lvls <- colnames(factors)[assign]
  names(lvls) <- colnames(mm)

  out <- lapply(colnames(factors), function(nm2) {
    idx <- factors[, nm2] > 0
    nm <- names(mf)[idx]
    labelEff <- vapply(nm, function(nam) {
      if(is.null(lab <- attr(mf[[nam]], "label"))) lab <- nam
      lab
    }, NA_character_)
    list(variable = paste(nm, collapse = ":"), variable2 = nm2,
         varterm = nm, varterm2 = row.names(factors)[idx], varlabel = unname(labelEff),
         term = names(lvls)[lvls == nm2])
  })
  names(out) <- vapply(out, "[[", NA_character_, "variable")
  out
}



modelsum_guts <- function(fam, temp.call, envir, conf.level, scope, anyna)
{
  check_pkg("broom")

  try_lrt <- function(f, s, a)
  {
    if(a) return(NA_real_)
    out <- setdiff(stats::drop1(f, scope = s, test = "Chisq")[["Pr(>Chi)"]], NA_real_)
    if(length(out) == 1) out else NA_real_
  }

  ## y is ordered factor
  if (fam$family == "ordinal") {
    check_pkg("MASS")
    temp.call[[1]] <- quote(MASS::polr)
    temp.call$Hess <- TRUE
    temp.call$method <- fam$method
    fit <- eval(temp.call, envir)
    coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=conf.level)
    coeffORTidy[coeffORTidy$coef.type != "coefficient", names(coeffORTidy) %nin% c("term", "coef.type")] <- NA
    coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=conf.level)
    coeffTidy$p.value <- 2*stats::pnorm(abs(coeffTidy$statistic), lower.tail = FALSE)
    coeffTidy <- cbind(coeffTidy, OR=coeffORTidy$estimate, CI.lower.OR=coeffORTidy$conf.low, CI.upper.OR=coeffORTidy$conf.high)
    # sort so that zeta comes first, but hold all else fixed
    coeffTidy <- coeffTidy[order(coeffTidy$coef.type == "coefficient", seq_len(nrow(coeffTidy))), ]
    modelGlance <- broom::glance(fit)
    modelGlance$p.value.lrt <- try_lrt(fit, scope, anyna)

  } else if (fam$family == "gaussian") {
    # ## issue warning if appears categorical

    temp.call[[1]] <- quote(stats::lm)
    temp.call$x <- TRUE
    fit <- eval(temp.call, envir)
    coeffTidy <- broom::tidy(fit, conf.int=TRUE, conf.level=conf.level)

    if("(weights)" %in% colnames(fit$model)) fit$model[["(weights)"]] <- NULL

    coeffTidy$standard.estimate <- lm.beta(fit)
    ## Continuous variable (numeric) ###############
    ## Note: Using tidy changes colname from 't value' to 'statistic'
    modelGlance <- broom::glance(fit)
    names(modelGlance)[names(modelGlance) == "statistic"] <- "statistic.F"
    names(modelGlance)[names(modelGlance) == "p.value"] <- "p.value.F"
    modelGlance$p.value.lrt <- try_lrt(fit, scope, FALSE)

  } else if (fam$family == "binomial" || fam$family == "quasibinomial") {
    ## These families are used in glm

    check_pkg("pROC")
    temp.call[[1]] <- quote(stats::glm)
    temp.call$x <- TRUE
    temp.call$family <- fam
    fit <- eval(temp.call, envir)
    #coeffbeta <- summary(fit)$coef
    ## find out that broom:::tidy.lm allows conf.int and exp
    coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=conf.level)
    coeffORTidy[coeffORTidy$term == "Intercept", -1] <- NA
    coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=conf.level)

    waldTidy <- suppressMessages(stats::confint.default(fit, conf.level=conf.level))
    all_na <- apply(waldTidy, 1, allNA)
    waldTidy <- stats::setNames(as.data.frame(waldTidy[!all_na, , drop = FALSE]), c("conf.low", "conf.high"))

    coeffTidy <- cbind(coeffTidy, OR=coeffORTidy$estimate, CI.lower.OR=coeffORTidy$conf.low, CI.upper.OR=coeffORTidy$conf.high,
                       CI.lower.wald=waldTidy$conf.low, CI.upper.wald=waldTidy$conf.high,
                       CI.lower.OR.wald=exp(waldTidy$conf.low), CI.upper.OR.wald=exp(waldTidy$conf.high))
    modelGlance <- broom::glance(fit)
    modelGlance$concordance <- as.numeric(pROC::auc(fit$y ~ predict(fit, type='response'), direction = "<", levels = 0:1))
    modelGlance$p.value.lrt <- try_lrt(fit, scope, FALSE)

  } else if (fam$family == "quasipoisson" || fam$family == "poisson") {
    ## These families use glm

    temp.call[[1]] <- quote(stats::glm)
    temp.call$x <- TRUE
    temp.call$family <- fam
    fit <- eval(temp.call, envir)
    coeffRRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=conf.level)
    coeffRRTidy[coeffRRTidy$term == "Intercept", -1] <- NA
    coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=conf.level)
    coeffTidy <- cbind(coeffTidy, RR=coeffRRTidy$estimate, CI.lower.RR=coeffRRTidy$conf.low, CI.upper.RR=coeffRRTidy$conf.high)
    modelGlance <- broom::glance(fit)
    modelGlance$p.value.lrt <- try_lrt(fit, scope, FALSE)

  } else if (fam$family == "negbin") {
    ## Also uses glm
    check_pkg("MASS")
    temp.call[[1]] <- quote(MASS::glm.nb)
    temp.call$x <- TRUE
    temp.call$link <- fam$link
    fit <- eval(temp.call, envir)
    coeffRRTidy <- suppressWarnings(broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=conf.level))
    coeffRRTidy[coeffRRTidy$term == "Intercept", -1] <- NA
    coeffTidy <- suppressWarnings(broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=conf.level))
    coeffTidy <- cbind(coeffTidy, RR=coeffRRTidy$estimate, CI.lower.RR=coeffRRTidy$conf.low, CI.upper.RR=coeffRRTidy$conf.high)
    modelGlance <- suppressWarnings(broom::glance(fit))
    modelGlance$theta <- fit$theta
    modelGlance$SE.theta <- fit$SE.theta
    modelGlance$p.value.lrt <- try_lrt(fit, scope, anyna)

  } else if(fam$family == "clog") {

    check_pkg("survival")
    temp.call[[1]] <- quote(survival::clogit)
    fit <- eval(temp.call, envir)
    ## use tidy to get both CIs, merge
    coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=conf.level)
    coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=conf.level)
    coeffTidy <- cbind(coeffTidy, OR=coeffORTidy$estimate, CI.lower.OR=coeffORTidy$conf.low, CI.upper.OR=coeffORTidy$conf.high)
    modelGlance <-  broom::glance(fit)
    names(modelGlance)[names(modelGlance) == "nevent"] <- "Nevents"
    modelGlance$p.value.lrt <- try_lrt(fit, scope, anyna)

  } else if (fam$family == "relrisk") {

    check_pkg("geepack")
    temp.call[[1]] <- quote(geepack::geeglm)
    temp.call$family <- stats::poisson(fam$link)
    temp.call$corstr <- "independence"
    fit <- eval(temp.call, envir)
    coeffRRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=conf.level)
    coeffRRTidy[coeffRRTidy$term == "Intercept", -1] <- NA
    coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=conf.level)
    coeffTidy <- cbind(coeffTidy, RR=coeffRRTidy$estimate, CI.lower.RR=coeffRRTidy$conf.low, CI.upper.RR=coeffRRTidy$conf.high)
    modelGlance <- broom::glance(fit)

  } else if(fam$family == "survival") {

    check_pkg("survival")
    temp.call[[1]] <- quote(survival::coxph)
    fit <- eval(temp.call, envir)
    ## use tidy to get both CIs, merge
    coeffHRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=conf.level)
    coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=conf.level)
    coeffTidy <- cbind(coeffTidy, HR=coeffHRTidy$estimate, CI.lower.HR=coeffHRTidy$conf.low, CI.upper.HR=coeffHRTidy$conf.high)
    modelGlance <-  broom::glance(fit)
    names(modelGlance)[names(modelGlance) == "nevent"] <- "Nevents"
    modelGlance$p.value.lrt <- try_lrt(fit, scope, anyna)
  }

  names(coeffTidy)[names(coeffTidy) == "conf.low"] <- "CI.lower.estimate"
  names(coeffTidy)[names(coeffTidy) == "conf.high"] <- "CI.upper.estimate"
  modelGlance[] <- lapply(modelGlance, unname)
  list(coeffTidy = coeffTidy, modelGlance = modelGlance, fit = fit)
}

