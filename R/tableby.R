
#' Summary Statistics of a Set of Independent Variables by a Categorical Variable
#'
#' Summarize one or more variables (x) by a categorical variable (y). Variables
#'   on the right side of the formula, i.e. independent variables, are summarized by the
#'   levels of a categorical variable on the left of the formula. Optionally, an appropriate test is performed to test the
#'   distribution of the independent variables across the levels of the categorical variable.
#'
#' @param formula an object of class \code{\link{formula}}; a symbolic description of the variables to be summarized by the group,
#'   or categorical variable, of interest. See "Details" for more information. To only view overall summary
#'   statistics, a one-sided formula can be used.
#' @param data an optional data frame, list or environment (or object coercible by \code{\link{as.data.frame}} to a data frame)
#'   containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)},
#'   typically the environment from which the function is called.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s.
#'   The default is \code{na.tableby(TRUE)} if there is a by-variable, and \code{na.tableby(FALSE)} if there is not.
#'   This schema thus includes observations with \code{NA}s in x variables,
#'   but removes those with \code{NA} in the categorical group variable and strata (if used).
#' @param subset an optional vector specifying a subset of observations (rows of data) to be used in the results.
#'   Works as vector of logicals or an index.
#' @param weights a vector of weights. Using weights will disable statistical tests.
#' @param strata a vector of strata to separate summaries by an additional group.
#' @param control control parameters to handle optional settings within \code{tableby}.
#'   Two aspects of \code{tableby} are controlled with these: test options of RHS variables across levels of the categorical
#'   grouping variable, and x variable summaries within the grouping variable. Arguments for \code{tableby.control}
#'   can be passed to \code{tableby} via the \code{...} argument, but if a control object and \code{...} arguments are both supplied,
#'   the latter are used. See \code{\link{tableby.control}} for more details.
#' @param ... additional arguments to be passed to internal \code{tableby} functions or \code{\link{tableby.control}}.
#' @param x an object of class \code{tableby}.
#'
#' @details
#' The group variable (if any) is categorical, which could be an integer, character,
#' factor, or ordered factor. \code{tableby} makes a simple summary of
#' the counts within the k-levels of the independent variables on the
#' right side of the formula. Note that unused levels are dropped.
#'
#' The \code{data} argument allows data.frames with label attributes for the columns, and those
#' labels will be used in the summary methods for the \code{tableby} class.
#'
#' The independent variables are a mixture of types: categorical (discrete),
#' numeric (continuous), and time to event (survival). These variables
#' are split by the levels of the group variable (if any), then summarized within
#' those levels, specific to the variable type. A statistical test is
#' performed to compare the distribution of the independent variables across the
#' levels of the grouping variable.
#'
#' The tests differ by the independent variable type, but can be specified
#' explicitly in the formula statement or in the control function.
#' These tests are accepted:
#' \itemize{
#'   \item{
#'     \code{anova}: analysis of variance test; the default test for continuous variables. When
#'     LHS variable has two levels, equivalent to two-sample t-test.
#'   }
#'   \item{
#'     \code{kwt}: Kruskal-Wallis Rank Test, optional test for continuous
#'     variables. When LHS variable has two levels, equivalent to Wilcoxon test.
#'   }
#'   \item{
#'     \code{chisq}: chi-square goodness of fit test for equal counts of a
#'     categorical variable across categories; the default for categorical
#'     or factor variables
#'   }
#'   \item{
#'     \code{fe}: Fisher's exact test for categorical variables
#'   }
#'   \item{
#'     \code{trend}: trend test for equal distribution of an ordered variable
#'     across a categorical variable; the default for ordered factor variables
#'   }
#'   \item{
#'     \code{logrank}: log-rank, the default for time-to-event variables
#'   }
#'   \item{
#'     \code{notest}: no test is performed.
#'   }
#' }
#'
#' To perform a mixture of asymptotic and rank-based tests on two
#' different continuous variables, an example formula is:
#' \code{formula = group ~ anova(age) + kwt(height)}. The test settings
#' in \code{tableby.control} apply to all independent variables of a given type.
#'
#' The summary statistics reported for each independent variable within the
#' group variable can be set in \code{\link{tableby.control}}.
#'
#' Finally, multiple by-variables can be set using \code{list()}. See the examples for more details.
#'
#' @return An object with class \code{'tableby'}
#' @seealso \code{\link[stats]{anova}}, \code{\link[stats]{chisq.test}}, \code{\link{tableby.control}},
#'   \code{\link{summary.tableby}}, \code{\link{formulize}}
#'
#' @examples
#' data(mockstudy)
#' tab1 <- tableby(arm ~ sex + age, data=mockstudy)
#' summary(tab1, text=TRUE)
#'
#' mylabels <- list(sex = "SEX", age ="Age, yrs")
#' summary(tab1, labelTranslations = mylabels, text=TRUE)
#'
#' tab3 <- tableby(arm ~ sex + age, data=mockstudy, test=FALSE, total=FALSE,
#'                 numeric.stats=c("median","q1q3"), numeric.test="kwt")
#' summary(tab3, text=TRUE)
#'
#' # multiple LHS
#' summary(tableby(list(arm, sex) ~ age, data = mockstudy, strata = ps), text = TRUE)
#'
#' tab.test <- tableby(arm ~ kwt(age) + anova(bmi) + kwt(ast), data=mockstudy)
#' tests(tab.test)
#'
#' @author Jason Sinnwell, Beth Atkinson, Gregory Dougherty, and Ethan Heinzen, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
#' @name tableby
NULL
#> NULL

#' @rdname tableby
#' @export
tableby <- function(formula, data, na.action, subset=NULL, weights=NULL, strata, control = NULL, ...)
{
  control <- c(list(...), control)
  control <- do.call("tableby.control", control[!duplicated(names(control))])

  Call <- match.call()
  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula", "data", "na.action", "subset", "weights", "strata", "control", names(control), "times")
  match.idx <- match(names(Call)[-1], expectArgs)
  if(anyNA(match.idx)) warning("unused arguments: ", paste(names(Call)[1+which(is.na(match.idx))], collapse=", "), "\n")

  indx <- match(c("formula", "data", "subset", "weights", "na.action", "strata"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required")

  special <- c("anova", "kwt", "chisq", "fe", "logrank", "trend", "notest")

  out.tables <- list()
  formula.list <- as_list_formula(formula)
  for(FORM in formula.list)
  {
    temp.call <- Call[c(1, indx)]
    temp.call[[1]] <- as.name("model.frame")

    if(is.null(temp.call$na.action)) {
      temp.call$na.action <- if(length(FORM) == 2) na.tableby(FALSE) else na.tableby(TRUE)
    } else if(length(FORM) == 2 && identical(na.action, na.tableby(TRUE), ignore.environment = TRUE)) {
      # purposely using na.action instead of temp.call$na.action here
      warning("It appears you're using na.tableby(TRUE) with a one-sided formula... Results may not be what you expect.")
    }

    if (missing(data)) {
      temp.call$formula <- stats::terms(FORM, special)
    } else {
      # instead of call("keep.labels", ...), which breaks when arsenal isn't loaded (Can't find "keep.labels")
      temp.call$data <- as.call(list(keep.labels, temp.call$data))
      temp.call$formula <- stats::terms(FORM, special, data = keep.labels(data))
    }

    ##  set up new environment for
    ## if specials, assign dummy versions of those functions
    tabenv <- new.env(parent = environment(formula))
    for(sp in special)
    {
      if(!is.null(attr(temp.call$formula, "specials")[[sp]])) assign(sp, inline_tableby_stat_test, envir = tabenv)
    }

    ## set tabenv as environment in which to evalulate formula
    environment(temp.call$formula) <- tabenv

    ## evaluate the formula with env set for it
    modeldf <- eval.parent(temp.call)
    if(nrow(modeldf) == 0) stop("No (non-missing) observations")

    Terms <- stats::terms(modeldf)

    ###### Check for weights ######

    if(hasWeights <- "(weights)" %in% colnames(modeldf)) {
      weights <- as.vector(stats::model.weights(modeldf))
      if(!is.numeric(weights) || any(weights < 0)) stop("'weights' must be a numeric vector and must be non-negative")
      modeldf[["(weights)"]] <- NULL
      control$test <- FALSE
    } else weights <- rep(1, nrow(modeldf))

    ###### Check for strata ######

    if(hasStrata <- "(strata)" %in% colnames(modeldf))
    {
      strata.col <- modeldf[["(strata)"]]
      strataTerm <- deparse(Call$strata)
      if(is.null(strataLabel <- attr(strata.col, "label"))) strataLabel <- strataTerm
      if(is.factor(strata.col))
      {
        strata.col <- droplevels(strata.col)
        strata.levels <- levels(strata.col)
      } else strata.levels <- sort(unique(strata.col))

      modeldf[["(strata)"]] <- NULL
    } else
    {
      strata.col <- rep("", nrow(modeldf))
      strataTerm <- strataLabel <- strata.levels <- ""
    }

    ###### Check for by-variable ######

    if(attributes(Terms)$response != 0)
    {
      by.col <- modeldf[[1]]
      termBy <- names(modeldf)[1]
      if(is.null(labelBy <- attr(by.col, "label"))) labelBy <- termBy
      if(is.factor(by.col)) {
        by.col <- droplevels(by.col)
        by.levels <- levels(by.col)
      } else by.levels <- sort(unique(by.col))
      by.col <- as.character(by.col)
      by.levels <- as.character(by.levels)

      if(any(by.levels == ""))
      {
        warning('Empty string detected in by-variable is not allowed; converting to " ".')
        by.col[by.col == ""] <- " "
        by.levels <- unique(replace(by.levels, by.levels == "", " "))
      }
      if(length(by.levels) < 2 && control$test)
      {
        warning("The by-variable has fewer than two levels; statistical tests are ignored")
        control$test <- FALSE
      }
      modeldf[[1]] <- NULL
    } else
    {
      ## no response, create a dummy one
      by.col <- rep("Overall", nrow(modeldf))
      termBy <- labelBy <- by.levels <- "Overall"
      control$total <- FALSE
      control$test <- FALSE
    }


    yList <- list(stats=c(table(factor(by.col, levels=by.levels), exclude=NA), Total=sum(!is.na(by.col))),
                  label=labelBy, term=termBy)

    ## find which columnss of modeldf have specials assigned to known specials
    specialIndices <- unlist(attr(Terms, "specials")) - attributes(Terms)$response
    specialTests <- rep("", ncol(modeldf))
    ## If a special shows up multiple times, the unlist assigned a number at the end. Strip it off.
    ## This disallows functions with a number at the end
    specialTests[specialIndices] <- gsub("\\d+$", "", names(specialIndices))

    xTerms <- Map(modeldf, names(modeldf), f = function(col, nm) {
      if(is.null(nameEff <- attr(col, "name"))) nameEff <- nm
      if(is.null(labelEff <- attr(col, "label"))) labelEff <- nameEff
      list(variable=nameEff, label=labelEff, term=nm)
    })
    names(xTerms) <- vapply(xTerms, "[[", NA_character_, "variable")

    control.list <- lapply(modeldf, attr, "control.list")
    names(control.list) <- names(xTerms)

    strataList <- vector("list", length(strata.levels))
    if(strataTerm != "") names(strataList) <- paste0("(", strataTerm, ") == ", strata.levels)

    for(strat in strata.levels)
    {
      ## list of x variables
      xList <- vector("list", ncol(modeldf))
      names(xList) <- names(xTerms)

      bycol <- by.col[strata.col == strat]
      weightscol <- weights[strata.col == strat]

      for(eff in seq_along(modeldf)) {

        currcol <- modeldf[[eff]]

        ############################################################

        if(is.ordered(currcol) || is.logical(currcol) || is.factor(currcol) || is.character(currcol)) {
          ######## ordinal or categorical variable (character or factor) ###############

          ## convert logicals and characters to factor
          if(is.character(currcol))
          {
            currcol <- factor(currcol, levels = sort(unique(currcol[!is.na(currcol)])))
          } else if(is.logical(currcol)) currcol <- factor(currcol, levels=c(FALSE, TRUE))

          ## to make sure all levels of cat variable are counted, need to pass values along
          xlevels <- levels(currcol)

          if(length(xlevels) == 0) stop(paste0("Zero-length levels found for ", names(xTerms)[eff]))

          ## get stats funs from either formula  or control
          if(is.ordered(currcol))
          {
            currstats <-  control$ordered.stats
            currtest <- control$ordered.test
            vartype <- "ordinal"
          } else
          {
            currstats <- control$cat.stats
            currtest <- control$cat.test
            vartype <- "categorical"
          }

        } else if(is.Date(currcol)) {
          ######## Date variable ###############
          xlevels <- sort(unique(currcol))

          ## get stats funs from either formula  or control
          currstats <- control$date.stats
          currtest <- control$date.test
          vartype <- "Date"

        } else if(survival::is.Surv(currcol)) {
          ##### Survival (time to event) #######
          xlevels <- NULL
          if(any(currcol[, 2] %nin% 0:1)) stop("Survival endpoint may not be coded 0/1.")

          currstats <- control$surv.stats
          currtest <- control$surv.test
          vartype <- "survival"

        } else if(is.numeric(currcol) || inherits(currcol, "difftime")) {
          ######## Continuous variable (numeric) ###############

          ## for difftime, convert to numeric
          if(inherits(currcol, "difftime")) currcol <- as.numeric(currcol)
          xlevels <- sort(unique(currcol))

          ## if no missings, and control says not to show missings,
          ## remove Nmiss stat fun
          currstats <- control$numeric.stats
          currtest <- control$numeric.test
          vartype <- "numeric"
        }
        ############################################################

        ## if no missings, and control says not to show missings,
        ## remove Nmiss stat fun
        if(!is.null(attrstats <- attr(modeldf[[eff]], "stats"))) currstats <- attrstats

        # now finally subset
        currcol <- currcol[strata.col == strat]
        if(!anyNA(currcol) && "Nmiss" %in% currstats) currstats <- currstats[currstats != "Nmiss"]
        statList <- list()
        for(statfun in currstats) {
          bystatlist <- list()
          if(statfun %in% c("countrowpct", "countcellpct", "rowbinomCI"))
          {
            bystatlist <- do.call(statfun, list(currcol, levels = xlevels,
                                                by = bycol, by.levels = by.levels, weights = weightscol, na.rm = TRUE))
          } else
          {
            for(bylev in by.levels) {
              idx <- bycol == bylev
              bystatlist[[bylev]] <- do.call(statfun, list(currcol[idx], levels=xlevels, na.rm=TRUE, weights=weightscol[idx], ...))
            }
            ## add Total
            bystatlist$Total <- do.call(statfun, list(currcol, levels=xlevels, weights=weightscol, ...))
          }
          statList[[statfun]] <- bystatlist
        }

        currtest <- if(nchar(specialTests[eff]) > 0) specialTests[eff] else currtest
        testout <- if(control$test) {
          eval(call(currtest, currcol, factor(bycol, levels = by.levels),
                    chisq.correct=control$chisq.correct, simulate.p.value=control$simulate.p.value, B=control$B))
        } else NULL

        xList[[eff]] <- list(stats=statList, test=testout, type=vartype)
      }

      strataList[[if(strataTerm == "") 1 else paste0("(", strataTerm, ") == ", strat)]] <- xList
    }
    out.tables[[termBy]] <- list(y = yList, strata = list(term = strataTerm, values = strata.levels, label = strataLabel),
                                 x = xTerms, tables = strataList, control.list = control.list)
  }
  structure(list(Call = Call, control = control, tables = out.tables, hasWeights = hasWeights, hasStrata = hasStrata), class = "tableby")
}
