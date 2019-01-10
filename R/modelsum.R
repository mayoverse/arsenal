
#' Fit models over each of a set of independent variables with a response variable
#'
#' Fit and summarize models for each independent (x) variable with a response variable (y), with options to adjust by variables for each model.
#'
#' @param formula an object of class \code{\link{formula}}; a symbolic description of the variables to be modeled.  See "Details" for more information.
#' @param adjust an object of class \code{\link{formula}}, listing variables to adjust by in all models. Specify as a one-sided formula,
#'   like: \code{~Age+ Sex}.
#' @param family similar mechanism to \code{\link[stats]{glm}}, where the model to be fit is driven by the family.
#'   Options include: binomial, gaussian, survival, poisson, negbin, and ordinal. These can be passed as a string, as a function,
#'   or as a list resulting from a call to one of the functions. See \code{\link{modelsum.family}} for details on
#'   survival and ordinal families.
#' @param data an optional data.frame, list or environment (or object coercible by \code{\link[base]{as.data.frame}} to a data frame) containing the
#'   variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically
#'   the environment from which \code{modelsum} is called.
#' @param subset an optional vector specifying a subset of observations (rows of \code{data}) to be used in the results.
#'   If \code{strata} is missing, this works as vector of logicals or an index; otherwise, it should be a logical vector.
#' @param weights an optional vector specifying the weights to apply to each data observation (rows of \code{data})
#' @param strata a vector of strata to separate model summaries by an additional group.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s.
#'   The default (\code{NULL}) is to use the defaults of \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, or \code{\link[survival]{coxph}},
#'   depending on the \code{family} specifications.
#' @param control control parameters to handle optional settings within \code{modelsum}.  Arguments for \code{modelsum.control}
#'   can be passed to \code{modelsum} via the \code{...} argument, but if a control object and \code{...} arguments are both supplied,
#'   the latter are used. See \code{\link{modelsum.control}} for other details.
#' @param ... additional arguments to be passed to internal \code{modelsum} functions.
#' @return An object with class \code{c("modelsum", "arsenal_table")}
#' @author Jason Sinnwell, Patrick Votruba, Beth Atkinson, Gregory Dougherty, and Ethan Heinzen, adapted from SAS Macro of the same name
#' @seealso \code{\link{arsenal_table}}, \code{\link{modelsum.control}}, \code{\link{summary.modelsum}}, \code{\link{formulize}}
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
                     subset=NULL, weights=NULL, strata, control = NULL, ...) {
  Call <- match.call()

  ## Allow family parameter to passed with or without quotes
  ## Here, we force quotes to simplify in for loop below
  if(is.function(family) || is.character(family))
  {
    family.list <- match.fun(family)()
    family <- family.list$family
  } else
  {
    family.list <- family
    family <- family$family
  }

  if(family %nin% c("survival", "gaussian", "binomial", "poisson", "quasibinomial", "quasipoisson", "ordinal", "negbin"))
    stop("Family ", family, " not supported.\n")

  if(family != "survival" && any(grepl("Surv\\(", formula))) {
    warning("Found Surv in formula, assuming family='survival'\n")
    family <- "survival"
  }
  ## pick up extra control arguments from command via ...
  control <- c(list(...), control)
  control <- do.call("modelsum.control", control[!duplicated(names(control))])

  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula", "family", "adjust", "data", "na.action", "subset", "weights", "strata", "control", names(control))
  match.idx <- match(names(Call)[-1], expectArgs)
  if(anyNA(match.idx)) warning("Unused arguments: ", paste(names(Call)[c(FALSE, is.na(match.idx))], collapse=", "), "\n")

  #### Set up "main effects" dataset ####
  indx.main <- match(c("formula", "data", "subset", "weights", "strata"), names(Call), 0L)
  if(indx.main[1] == 0) stop("A formula argument is required")
  if(length(formula) == 2) stop("'formula' should have a response variable!")
  if(!is.null(adjust) && length(adjust) != 2) stop("'adjust' shouldn't have a response variable!")

  is.numericish <- function(x) is.numeric(x) || is.Date(x)

  get_terms_with_contrasts <- function(trms, col, coeff, contrs)
  {
    if(!is.numeric(col) && !is.Date(col))
    {
      lvls <- unique(col)
      findlvls <- if(identical(contrs[[trms$variable]], "contr.treatment"))
      {
        paste0(trms$variable2, lvls)
      } else if(identical(contrs[[trms$variable]], "contr.poly"))
      {
        paste0(trms$variable2, c(".L", ".Q", ".C", paste0("^", seq_along(lvls))))
      } else paste0(trms$variable2, seq_along(lvls))

      trms$term <- union(trms$term, coeff$term[coeff$term %in% findlvls])
    } else trms$term <- union(trms$term, trms$variable2)

    trms
  }

  out.tables <- list()
  formula.list <- as_list_formula(formula)
  for(FORM in formula.list)
  {
    main.call <- Call[c(1, indx.main)]
    main.call[[1]] <- quote(stats::model.frame)
    main.call$formula <- FORM
    main.call$na.action <- quote(stats::na.pass) # for now, keep all rows, except for what is subset out
    if(!missing(data))
    {
      # instead of call("keep.labels", ...), which breaks when arsenal isn't loaded (Can't find "keep.labels")
      main.call$data <- as.call(list(keep.labels, main.call$data))
    }
    maindf <- eval(main.call, parent.frame())
    if(nrow(maindf) == 0) stop("No (non-missing) observations")
    Terms <- stats::terms(maindf)

    #### Check for weights ####
    if(hasWeights <- "(weights)" %in% colnames(maindf)) maindf[["(weights)"]] <- NULL

    #### Check for strata ####
    if(hasStrata <- "(strata)" %in% colnames(maindf))
    {
      strata.col <- maindf[["(strata)"]]
      strataTerm <- deparse(Call$strata)
      if(is.null(strataLabel <- attr(strata.col, "label"))) strataLabel <- strataTerm
      if(is.factor(strata.col))
      {
        strata.col <- droplevels(strata.col)
        strata.levels <- levels(strata.col)
      } else strata.levels <- sort(unique(strata.col))

      maindf[["(strata)"]] <- NULL
    } else
    {
      strata.col <- rep("", nrow(maindf))
      strataTerm <- strataLabel <- strata.levels <- ""
    }

    #### Set up "adjustment" dataset ####
    if(missing(adjust))
    {
      adjustdf <- NULL
      adjTerms <- NULL
    } else
    {
      adj.call <- main.call
      adj.call$formula <- adjust
      adj.call$weights <- NULL
      adj.call$strata <- NULL
      adjustdf <- eval(adj.call, parent.frame())

      Terms.a <- stats::terms(adjustdf)
      if(!is.null(attr(Terms.a, "offset")))
      {
        adjustdf[attr(Terms.a, "offset")] <- NULL
      }

      adjTerms <- Map(adjustdf, names(adjustdf), attr(stats::terms(adjustdf), "term.labels"), f = function(col, nm, nm2) {
        if(is.null(labelEff <- attr(col, "label"))) labelEff <- nm
        list(variable=nm, variable2=nm2, varlabel=labelEff)
      })
      names(adjTerms) <- vapply(adjTerms, "[[", NA_character_, "variable")
    }

    #### Get info on y-variable ####
    yCol <- maindf[[1]]
    yTerm <- colnames(maindf)[1]
    if(is.null(yLabel <- attr(yCol, "label"))) yLabel <- yTerm
    yList <- list(label = yLabel, term = yTerm)
    maindf[[1]] <- NULL


    #### Now finish the x-variables ####
    effCols <- seq_along(maindf)

    xTerms <- Map(maindf, names(maindf), attr(Terms, "term.labels"), f = function(col, nm, nm2) {
      if(is.null(labelEff <- attr(col, "label"))) labelEff <- nm
      list(variable=nm, variable2=nm2, varlabel=labelEff)
    })
    names(xTerms) <- vapply(xTerms, "[[", NA_character_, "variable")




    strataList <- vector("list", length(strata.levels))
    if(hasStrata) names(strataList) <- paste0("(", strataTerm, ") == ", strata.levels)

    for(strat in strata.levels)
    {
      xList <- vector("list", length(effCols))
      names(xList) <- names(xTerms)
      idx <- if(!hasStrata) NULL else call("==", call("(", Call$strata), strat)

      for(eff in effCols)
      {
        currCol <- maindf[[eff]]
        adj.formula <- join_formula(stats::drop.terms(Terms, if(length(effCols) > 1) setdiff(effCols, eff) else NULL, keep.response = TRUE), adjust)

        temp.call <- Call[c(1, match(c("data", "subset", "na.action", "weights"), names(Call), 0L))]
        temp.call$formula <- adj.formula
        if(hasStrata)
        {
          temp.call$subset <- if(!is.null(temp.call$subset)) call("&", call("(", temp.call$subset), idx) else idx
        }

        ## y is ordered factor
        if (family == "ordinal") {
          temp.call[[1]] <- quote(MASS::polr)
          temp.call$Hess <- TRUE
          temp.call$method <- family.list$method
          fit <- eval(temp.call, parent.frame())
          coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
          coeffORTidy[coeffORTidy$coefficient_type == "zeta", names(coeffORTidy) %nin% c("term", "coefficient_type")] <- NA
          coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)
          coeffTidy$p.value <- 2*stats::pnorm(abs(coeffTidy$statistic), lower.tail = FALSE)
          coeffTidy <- cbind(coeffTidy, OR=coeffORTidy$estimate, CI.lower.OR=coeffORTidy$conf.low, CI.upper.OR=coeffORTidy$conf.high)
          # sort so that zeta comes first, but hold all else fixed
          coeffTidy <- coeffTidy[order(coeffTidy$coefficient_type == "coefficient", seq_len(nrow(coeffTidy))), ]
          modelGlance <- broom::glance(fit)

        } else if (family == "gaussian") {
          # ## issue warning if appears categorical
          if(length(unique(yCol)) <= 5)
            warning("Input family=gaussian, but dependent variable has 5 or fewer categories\n")

          temp.call[[1]] <- quote(stats::lm)
          temp.call$x <- TRUE
          fit <- eval(temp.call, parent.frame())
          coeffTidy <- broom::tidy(fit, conf.int=TRUE, conf.level=control$conf.level)

          if("(weights)" %in% colnames(fit$model)) fit$model[["(weights)"]] <- NULL

          coeffTidy$standard.estimate <- lm.beta(fit)
          ## Continuous variable (numeric) ###############
          ## Note: Using tidy changes colname from 't value' to 'statistic'
          modelGlance <- broom::glance(fit)
          names(modelGlance)[names(modelGlance) == "p.value"] <- "p.value.F"


        } else if (family == "binomial" || family == "quasibinomial") {
          ## These families are used in glm

          temp.call[[1]] <- quote(stats::glm)
          temp.call$x <- TRUE
          temp.call$family <- family.list
          fit <- eval(temp.call, parent.frame())

          rocOut <- pROC::roc(fit$y ~ predict(fit, type='response'))
          #coeffbeta <- summary(fit)$coef
          ## find out that broom:::tidy.lm allows conf.int and exp
          coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
          coeffORTidy[coeffORTidy$term == "Intercept", -1] <- NA
          coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)
          coeffTidy <- cbind(coeffTidy, OR=coeffORTidy$estimate, CI.lower.OR=coeffORTidy$conf.low, CI.upper.OR=coeffORTidy$conf.high)
          modelGlance <- c(broom::glance(fit), concordance = pROC::auc(rocOut))

        } else if (family == "quasipoisson" || family == "poisson") {
          ## These families use glm

          temp.call[[1]] <- quote(stats::glm)
          temp.call$x <- TRUE
          temp.call$family <- family.list
          fit <- eval(temp.call, parent.frame())

          coeffRRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
          coeffRRTidy[coeffRRTidy$term == "Intercept", -1] <- NA
          coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)
          coeffTidy <- cbind(coeffTidy, RR=coeffRRTidy$estimate, CI.lower.RR=coeffRRTidy$conf.low, CI.upper.RR=coeffRRTidy$conf.high)
          modelGlance <- broom::glance(fit)

        } else if (family == "negbin") {
          ## Also uses glm
          temp.call[[1]] <- quote(MASS::glm.nb)
          temp.call$x <- TRUE
          temp.call$link <- family.list$link
          fit <- eval(temp.call, parent.frame())

          coeffRRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
          coeffRRTidy[coeffRRTidy$term == "Intercept", -1] <- NA
          coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)
          coeffTidy <- cbind(coeffTidy, RR=coeffRRTidy$estimate, CI.lower.RR=coeffRRTidy$conf.low, CI.upper.RR=coeffRRTidy$conf.high)
          modelGlance <- broom::glance(fit)
          modelGlance$theta <- fit$theta
          modelGlance$SE.theta <- fit$SE.theta

        } else if(family == "survival") {

          temp.call[[1]] <- quote(survival::coxph)
          fit <- eval(temp.call, parent.frame())

          ## use tidy to get both CIs, merge
          coeffHRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=.95)
          coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=.95)
          coeffTidy <- cbind(coeffTidy, HR=coeffHRTidy$estimate, CI.lower.HR=coeffHRTidy$conf.low, CI.upper.HR=coeffHRTidy$conf.high)
          modelGlance <-  broom::glance(fit)
        }

        names(coeffTidy)[names(coeffTidy) == "conf.low"] <- "CI.lower.estimate"
        names(coeffTidy)[names(coeffTidy) == "conf.high"] <- "CI.upper.estimate"

        xTerms[[eff]] <- get_terms_with_contrasts(xTerms[[eff]], currCol, coeffTidy, fit$contrasts)

        for(adj in seq_along(adjustdf)) { ## manage adj terms and labels
          adjTerms[[adj]] <- get_terms_with_contrasts(adjTerms[[adj]], adjustdf[[adj]], coeffTidy, fit$contrasts)
        }

        xList[[eff]] <- list(
          coeff=coeffTidy,
          glance = c(
            modelGlance,
            N = sum(!is.na(currCol)),
            Nmiss = sum(is.na(currCol)),
            Nmiss2 = sum(is.na(currCol)),
            endpoint=yTerm,
            endlabel=yLabel,
            x=xTerms[[eff]]$variable,
            contrasts=list(fit$contrasts)
          )
        )
      }
      strataList[[if(!hasStrata) 1 else paste0("(", strataTerm, ") == ", strat)]] <- xList
    }

    out.tables[[yTerm]] <- list(y = yList, strata = list(term = strataTerm, values = strata.levels, label = strataLabel, hasStrata = hasStrata),
                                x = lapply(xTerms, make_ms_labs),
                                adjust = if(!is.null(adjTerms)) lapply(adjTerms, make_ms_labs) else adjTerms,
                                tables = strataList, family = family, hasWeights = hasWeights)
  }

  structure(list(Call = Call, control = control, tables = out.tables), class = c("modelsum", "arsenal_table"))
}
