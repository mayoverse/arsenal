## Purpose: multiple models from multiple y and x variables
## Author: P Votruba J Sinnwell and Beth Atkinson
## Created: 9/3/2015
## Updated: 10/6/2015
## Updated: 4/6/2016 to complete using of broom tidy and glance
## Updated: 4/12/2016 to make lm.beta work to skip categorical and psplines
## Updated: 5/19/2016 get labels and y~. and y~x1 working. subsets working.
## Updated: 6/28/2016 -label() works for assign and get, for x, adjust, and y.
##                    -Expanded labels for categorical adjust and x variables.
## Updated: 7/25/2016 bug fix for when multiple data columns match y name

## examples now in modelsum.Rd and test.modelsum.R and modelsum.Rmd vignette

#' Fit models over each of a set of independent variables with a response variable
#'
#' Fit and summarize models for each independent (x) variable with a response variable (y), with options to adjust by variables for each model.
#'
#' @param formula an object of class \code{\link{formula}}; a symbolic description of the variables to be modeled.  See "Details" for more information.
#' @param adjust an object of class \code{\link{formula}}, listing variables to adjust by in all models. Specify as a one-sided formula,
#'   like: \code{~Age+ Sex}.
#' @param family similar mechanism to \code{\link[stats]{glm}}, where the model to be fit is driven by the family, options include: binomial, gaussian, survival,
#'   Poisson. Family options supported in glm can be in quotes or not, but survival requires quotes.
#' @param data an optional data.frame, list or environment (or object coercible by \code{\link[base]{as.data.frame}} to a data frame) containing the
#'   variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically
#'   the environment from which \code{modelsum} is called.
#' @param subset an optional vector specifying a subset of observations (rows of \code{data}) to be used in the results.
#'   Works as vector of logicals or an index.
#' @param weights an optional vector specifying the weights to apply to each data observation (rows of \code{data})
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s.
#'   The default (\code{NULL}) is to use the defaults of \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, or \code{\link[survival]{coxph}},
#'   depending on the \code{family} specifications.
#' @param control control parameters to handle optional settings within \code{modelsum}.  Arguments for \code{modelsum.control}
#'   can be passed to \code{modelsum} via the \code{...} argument, but if a control object and \code{...} arguments are both supplied,
#'   the latter are used. See \code{\link{modelsum.control}} for other details.
#' @param ... additional arguments to be passed to internal \code{modelsum} functions.
#' @param x An object of class \code{'modelsum'}, or a list of such objects.
#' @return
#' An object with class \code{'modelsum'}, which is effectively a list with the variables from the right-side in x and the group variable in y.
#'   Then, each item in x has these:
#'   \item{fits}{a list with an item in X for each x in y ~ X + adjust variables}
#'   \item{family}{family used in glm}
#'   \item{Call}{Original call to modelsum}
#'   \item{control}{list of control parameters used in \code{modelsum}, and to be used in \code{\link{summary.modelsum}},
#'     the result of \code{\link{modelsum.control}}}
#' @author Jason Sinnwell, Patrick Votruba, Beth Atkinson, Gregory Dougherty, and Ethan Heinzen, adapted from SAS Macro of the same name
#' @seealso \code{\link{modelsum.control}}, \code{\link{summary.modelsum}}, \code{\link{formulize}}
#' @examples
#'
#' data(mockstudy)
#'
#' tab1 <- modelsum(bmi ~ sex + age, data = mockstudy)
#' summary(tab1, text = TRUE)
#'
#' tab2 <- modelsum(alk.phos ~ arm + ps + hgb, adjust = ~ age + sex,
#'                  family = "gaussian", data = mockstudy)
#' summary(tab2, text = TRUE)
#'
#' summary(tab2, show.intercept = FALSE, text = TRUE)
#'
#' tab2.df <- as.data.frame(tab2)
#'
#' tab2.df[1:5,]
#' @name modelsum
NULL
#> NULL

#' @rdname modelsum
#' @export

modelsum <- function(formula,  family="gaussian", data, adjust=NULL, na.action = NULL,
                     subset=NULL, weights=NULL, control = NULL, ...) {
  Call <- match.call()

  ## Allow family parameter to passed with or without quotes
  ##    exception is survival, would require public function named survival.
  ## Here, we force quotes to simplify in for loop below
  if (is.function(family)) family <- family()$family

  if(family %nin% c("survival", "gaussian", "binomial", "poisson", "quasibinomial", "quasipoisson"))
    stop("Family ", family, "not supported.\n")

  if(family != "survival" && any(grepl("Surv\\(", formula))) {
    warning("Found Surv in formula, assuming family='survival'\n")
    family <- "survival"
  }
  ## pick up extra control arguments from command via ...
  control <- c(list(...), control)
  control <- do.call("modelsum.control", control[!duplicated(names(control))])

  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula", "family", "adjust", "data", "na.action", "subset", "weights", "control", names(control))
  match.idx <- match(names(Call)[-1], expectArgs)
  if(anyNA(match.idx)) warning("Unused arguments: ", paste(names(Call)[c(FALSE, is.na(match.idx))], collapse=", "), "\n")

  #### Set up "main effects" dataset ####
  indx.main <- match(c("formula", "data", "subset"), names(Call), 0L)
  if(indx.main[1] == 0) stop("A formula argument is required")
  if(length(formula) == 2) stop("'formula' should have a response variable!")
  if(!is.null(adjust) && length(adjust) != 2) stop("'adjust' shouldn't have a response variable!")
  main.call <- Call[c(1, indx.main)]
  main.call[[1]] <- quote(stats::model.frame)
  main.call$na.action <- quote(stats::na.pass) # for now, keep all rows
  if(!missing(data))
  {
    # instead of call("keep.labels", ...), which breaks when arsenal isn't loaded (Can't find "keep.labels")
    main.call$data <- as.call(list(keep.labels, main.call$data))
  }
  maindf <- eval(main.call, parent.frame())
  if(nrow(maindf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(maindf)

  #### Set up "adjustment" dataset ####
  if(missing(adjust))
  {
    adjustdf <- NULL
  } else
  {
    adj.call <- main.call
    adj.call$formula <- adjust
    adjustdf <- eval(adj.call, parent.frame())
  }

  effCols <- seq_along(maindf)[-1]
  yTerm <- colnames(maindf)[1]
  yLabel <- attributes(maindf[[1]])$label
  if(is.null(yLabel)) yLabel <- yTerm
  fitList <- list()

  for(eff in effCols) {

    currCol <- maindf[[eff]]
    adj.formula <- join_formula(stats::drop.terms(Terms, if(length(effCols) > 1) setdiff(effCols, eff) - 1L else NULL, keep.response = TRUE), adjust)
    adj.Terms <- stats::terms(adj.formula)
    adjVars <- attr(adj.Terms, "term.labels")[-1]

    xname <- colnames(maindf)[eff]
    xname2 <- attr(adj.Terms, "term.labels")[1] # this one will have backticks for non-syntactic names
    labelEff <-  attributes(currCol)$label
    if(is.null(labelEff)) labelEff <- xname

    temp.call <- Call[c(1, match(c("data", "subset", "na.action", "weights"), names(Call), 0L))]
    temp.call$formula <- adj.formula

    ## placeholder for ordered, don't do any fitting
    ## y is ordered factor
    if (family == "ordered") {
      ## look into using same ordered test from tableby
      modelGlance <- list()

      fitList[[xname]] <- list(#coeff=summary(coeff(p(maindf[,1]~ currCol),
                               family="ordered", label=xname)
    } else if (family == "gaussian") {
      # ## issue warning if appears categorical
      if(length(unique(maindf[[1]])) <= 5) {
        warning("Input family=gaussian, but dependent variable has 5 or fewer categories\n")
      }
      temp.call[[1]] <- quote(stats::lm)
      temp.call$x <- TRUE
      lmfit <- eval(temp.call, parent.frame())
      coeffTidy <- broom::tidy(lmfit, conf.int=TRUE, conf.level=control$conf.level)

      if(any(grepl("(weights)", colnames(lmfit$model)))) {
        lmfit$model <- lmfit$model[,-grep("(weights)", colnames(lmfit$model))]
      }
      coeffTidy$standard.estimate <- lm.beta(lmfit)
      names(coeffTidy)[names(coeffTidy) == "conf.low"] <- "CI.lower.estimate"
      names(coeffTidy)[names(coeffTidy) == "conf.high"] <- "CI.upper.estimate"
      xterms <- coeffTidy$term[starts_with(coeffTidy$term, xname2)]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname2)) labelEff <- gsub(xname2, paste0(labelEff, " "), xterms, fixed = TRUE)

      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy$term[starts_with(coeffTidy$term, adj)]
        if(length(aterm) > 0)
        {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(adjustdf[[adj]])$label
          if(is.null(alabel)) alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj)) alabel <- gsub(adj, paste0(alabel, " "), aterm, fixed = TRUE)
          adjlabels <- c(adjlabels, alabel)
        }
      }

      ## Continuous variable (numeric) ###############
      ## Note: Using tidy changes colname from 't value' to 'statistic'
      modelGlance <- broom::glance(lmfit)
      names(modelGlance) <- gsub("p.value","p.value.F", names(modelGlance))
      fitList[[xname]] <- list(coeff=coeffTidy,
                               family="gaussian",
                               xterms=xterms, label=labelEff,
                               adjterms=adjterms, adjlabels=adjlabels)

    } else if (family == "binomial" || family == "quasibinomial") {
      ## These families are used in glm

      temp.call[[1]] <- quote(stats::glm)
      temp.call$x <- TRUE
      temp.call$family <- family
      fit <- eval(temp.call, parent.frame())

      rocOut <- pROC::roc(fit$y ~ predict(fit, type='response'))
      #coeffbeta <- summary(fit)$coef
      ## find out that broom:::tidy.lm allows conf.int and exp
      coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
      coeffORTidy[grep("Intercept",coeffORTidy$term),-1] <- NA
      coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)

      names(coeffTidy)[names(coeffTidy) == "conf.low"] <- "CI.lower.estimate"
      names(coeffTidy)[names(coeffTidy) == "conf.high"] <- "CI.upper.estimate"
      coeffTidy <- cbind(coeffTidy, OR=coeffORTidy$estimate, CI.lower.OR=coeffORTidy$conf.low, CI.upper.OR=coeffORTidy$conf.high)
      xterms <- coeffTidy$term[starts_with(coeffTidy$term, xname2)]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname2)) labelEff <- gsub(xname2, paste0(labelEff, " "), xterms, fixed = TRUE)


      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy$term[starts_with(coeffTidy$term, adj)]
        if(length(aterm) > 0)
        {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(adjustdf[[adj]])$label
          if(is.null(alabel)) alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj)) alabel <- gsub(adj, paste0(alabel, " "), aterm, fixed = TRUE)
          adjlabels <- c(adjlabels, alabel)
        }
      }
      ## tidy data frame has extra column for terms (row names), shift col index +1
      ## 'z value' changed to 'statistic'
      modelGlance <- c(broom::glance(fit), concordance = pROC::auc(rocOut))
      fitList[[xname]] <- list(coeff=coeffTidy,
                               family=family,
                               xterms=xterms, label=labelEff,
                               adjterms=adjterms, adjlabels=adjlabels)

    } else if (family == "quasipoisson" || family == "poisson") {
      ## These families use glm

      temp.call[[1]] <- quote(stats::glm)
      temp.call$x <- TRUE
      temp.call$family <- family
      fit <- eval(temp.call, parent.frame())

      ## find out that broom:::tidy.lm allows conf.int and exp
      coeffRRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
      coeffRRTidy[grep("Intercept",coeffRRTidy$term),-1] <- NA
      coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)

      names(coeffTidy)[names(coeffTidy) == "conf.low"] <- "CI.lower.estimate"
      names(coeffTidy)[names(coeffTidy) == "conf.high"] <- "CI.upper.estimate"

      coeffTidy <- cbind(coeffTidy, RR=coeffRRTidy$estimate, CI.lower.RR=coeffRRTidy$conf.low, CI.upper.RR=coeffRRTidy$conf.high)
      xterms <- coeffTidy$term[starts_with(coeffTidy$term, xname2)]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname2)) labelEff <- gsub(xname2, paste0(labelEff, " "), xterms, fixed = TRUE)


      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy$term[starts_with(coeffTidy$term, adj)]
        if(length(aterm) > 0)
        {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(adjustdf[[adj]])$label
          if(is.null(alabel)) alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj)) alabel <- gsub(adj, paste0(alabel, " "), aterm, fixed = TRUE)
          adjlabels <- c(adjlabels, alabel)
        }
      }

      ## tidy data frame has extra column for terms (row names), shift col index +1
      ## 'z value' changed to 'statistic'

      modelGlance <- broom::glance(fit)
      fitList[[xname]] <- list(coeff=coeffTidy,
                            family=family,
                            xterms=xterms, label=labelEff,
                            adjterms=adjterms, adjlabels=adjlabels)

    } else if(family=="survival") {

      temp.call[[1]] <- quote(survival::coxph)
      ph <- eval(temp.call, parent.frame())

      ## use tidy to get both CIs, merge
      coeffHRTidy <- broom::tidy(ph, exponentiate=TRUE, conf.int=.95)
      coeffTidy <- broom::tidy(ph, exponentiate=FALSE, conf.int=.95)

      names(coeffTidy)[names(coeffTidy) == "conf.low"] <- "CI.lower.estimate"
      names(coeffTidy)[names(coeffTidy) == "conf.high"] <- "CI.upper.estimate"

      coeffTidy <- cbind(coeffTidy, HR=coeffHRTidy$estimate, CI.lower.HR=coeffHRTidy$conf.low, CI.upper.HR=coeffHRTidy$conf.high)
      xterms <- coeffTidy$term[starts_with(coeffTidy$term, xname2)]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname2)) labelEff <- gsub(xname2, paste0(labelEff, " "), xterms, fixed = TRUE)


      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy$term[starts_with(coeffTidy$term, adj)]
        if(length(aterm) > 0)
        {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(adjustdf[[adj]])$label
          if(is.null(alabel)) alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj)) alabel <- gsub(adj, paste0(alabel, " "), aterm, fixed = TRUE)
          adjlabels <- c(adjlabels, alabel)
        }
      }

      ## work with fit to get hr, try summary(fit) as above
      modelGlance <-  broom::glance(ph)

      ## Survival (time to event) #######
      fitList[[xname]] <- list(coeff=coeffTidy,
                           family="survival",
                           xterms=xterms, label=labelEff,
                           adjterms=adjterms, adjlabels=adjlabels)
    }
## put xname and endpoint in glance, summary and as.data.frame to pull from there
    fitList[[xname]]$glance <- c(modelGlance,
                                 N = sum(!is.na(currCol)),
                                 Nmiss = sum(is.na(currCol)),
                                 Nmiss2 = sum(is.na(currCol)),
                                 endpoint=yTerm,
                                 endlabel=yLabel,
                                 x=xname)

  } # end for: eff

  msList <- list(fits=fitList, control = control, Call = Call, family=family)
                 ##, adjust=adjVars)
  class(msList) <- "modelsum"
  return(msList)
}

## Needed for being able to use "survival" with or without quotes,
##   keep as private function
survival <- function() list(family="survival")

#' @rdname modelsum
#' @export
print.modelsum <- function(x, ...) {
  cat("Modelsum S3 Object\n\n")
  cat("Function Call: \n")
  print(x$Call)
  cat("\n")
  cat("y variable:\n")
  print(x$fits[[1]]$glance$endpoint)
  cat("x variables:\n")
  print(unname(vapply(x$fits, function(tmp) tmp$glance$x, "")))
  invisible(x)
}

#' @rdname modelsum
#' @export
print.modelsumList <- function(x, ...) {
  lapply(x, print.modelsum, ...)
  invisible(x)
}
