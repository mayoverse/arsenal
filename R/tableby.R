
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
#'   The default is \code{na.tableby} if there is a by variable, and \code{\link[stats]{na.pass}} if there is not.
#'   This schema thus includes observations with \code{NA}s in x variables,
#'   but removes those with \code{NA} in the categorical group variable.
#' @param subset an optional vector specifying a subset of observations (rows of data) to be used in the results.
#'   Works as vector of logicals or an index.
#' @param weights a vector of weights.
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
#' tab.test <- tableby(arm ~ kwt(age) + anova(bmi) + kwt(ast), data=mockstudy)
#' tests(tab.test)
#' @author Jason Sinnwell, Beth Atkinson, Gregory Dougherty, and Ethan Heinzen, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
#' @name tableby
NULL
#> NULL

#' @rdname tableby
#' @export
tableby <- function(formula, data, na.action, subset=NULL, weights=NULL, control = NULL, ...) {

  control <- c(list(...), control)
  control <- do.call("tableby.control", control[!duplicated(names(control))])

  Call <- match.call()
  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula", "data", "na.action", "subset", "weights", "control", names(control), "times")
  match.idx <- match(names(Call)[-1], expectArgs)
  if(anyNA(match.idx)) warning("unused arguments: ", paste(names(Call)[1+which(is.na(match.idx))],collapse=", "), "\n")

  indx <- match(c("formula", "data", "subset", "weights", "na.action"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required")

  if(indx[4] != 0) {   ## weights
    control$test <- FALSE
  }
  temp.call <- Call[c(1, indx)]
  temp.call[[1]] <- as.name("model.frame")

  if(is.null(temp.call$na.action)) {
    temp.call$na.action <- if(length(formula) == 2) stats::na.pass else na.tableby
  } else if(length(formula) == 2 && identical(na.action, na.tableby)) {
    # purposely using na.action instead of temp.call$na.action here
    warning("It appears you're using na.tableby with a one-sided formula... Results may not be what you expect.")
  }
  special <- c("anova", "kwt", "chisq", "fe", "logrank", "trend")
  if (missing(data)) {
    temp.call$formula <- stats::terms(formula, special)
  } else {
    # instead of call("keep.labels", ...), which breaks when arsenal isn't loaded (Can't find "keep.labels")
    temp.call$data <- as.call(list(keep.labels, temp.call$data))
    temp.call$formula <- stats::terms(formula, special, data = keep.labels(data))
  }
  ##  set up new environment for
  ## if specials, assign dummy versions of those functions
  ## if(any(!is.null(attr(temp.call$formula, "specials"))))
  tabenv <- new.env(parent = environment(formula))

  ## allow stat functions to be passed as single arguments that are strings of function names
  ## Store this as attribute in the modeldf column, along with the actual name of the variable,
  ## rather than anova(age) showing up in the result (though anova(age) will be the column name in modeldf
  ## but we pull these attributes off later.
  tmp.fun <- function(x, ...) set_attr(set_attr(x, "name", deparse(substitute(x))), "stats", list(...))
  for(sp in special)
  {
    if(!is.null(attr(temp.call$formula, "specials")[[sp]])) assign(sp, tmp.fun, envir = tabenv)
  }

  ## set tabenv as environment in which to evalulate formula
  environment(temp.call$formula) <- tabenv

  ## evaluate the formula with env set for it
  modeldf <- eval.parent(temp.call)
  if (nrow(modeldf) == 0) stop("No (non-missing) observations")

  Terms <- stats::terms(modeldf)

  if(attributes(Terms)$response == 0) {
    ## no response, create a dummy one
    modeldf <- data.frame(Total="Overall",modeldf, stringsAsFactors=FALSE, check.names = FALSE)
    control$total <- FALSE
    control$test <- FALSE
  }
  weights <- as.vector(stats::model.weights(modeldf))
  if(is.null(weights)) {
    weights <- rep(1, nrow(modeldf))
    userWeights <- FALSE
  }
  if("(weights)" %in% colnames(modeldf)) {
    modeldf <- modeldf[colnames(modeldf) != "(weights)"]
    userWeights <-TRUE
  }
  if (!is.null(weights) && (!is.numeric(weights) || any(weights<0))) {
    stop("'weights' must be a numeric vector and must be non-negative")
  }

  ## find which columnss of modeldf have specials assigned to known specials
  specialIndices <- unlist(attr(Terms, "specials"))
  specialTests <- rep("", ncol(modeldf))
  ## If a special shows up multiple times, the unlist assigned a number at the end. Strip it off.
  ## This disallows functions with a number at the end
  specialTests[specialIndices] <- gsub("\\d+$", "", names(specialIndices))

  ## list of x variables
  xList <- list()

  ## fix of droplevels on by factor suggested by Ethan Heinzen 4/12/2016
  by.col <- modeldf[[1]]
  if(is.factor(by.col)) {
    by.col <- droplevels(by.col)
    by.levels <- levels(by.col)
  } else by.levels <- sort(unique(by.col))

  if(length(by.levels) < 2 && attributes(Terms)$response != 0)
  {
    warning("The by-variable has fewer than two levels; statistical tests are ignored")
    control$test <- FALSE
  }

  for(eff in 2:ncol(modeldf)) {

    currcol <- modeldf[[eff]]

    ## label
    nameEff <- attributes(currcol)$name
    if(is.null(nameEff))  nameEff <- names(modeldf)[eff]
    labelEff <-  attributes(currcol)$label
    if(is.null(labelEff))  labelEff <- nameEff
    statList <- list()
    bystatlist <- list()

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

      if(length(xlevels) == 0)
      {
        warning(paste0("Zero-length levels found for ", nameEff))
        next
      }

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
      xlevels <- NULL

      ## get stats funs from either formula  or control
      currstats <- control$date.stats
      currtest <- control$date.test
      vartype <- "Date"

    } else if(survival::is.Surv(currcol)) {
      ##### Survival (time to event) #######
      xlevels <- NULL

      currstats <- control$surv.stats
      currtest <- control$surv.test
      vartype <- "survival"

    } else if(is.numeric(currcol) || inherits(currcol, "difftime")) {
      ######## Continuous variable (numeric) ###############
      xlevels <- NULL

      ## for difftime, convert to numeric
      if(inherits(currcol, "difftime")) currcol <- as.numeric(currcol)

      ## if no missings, and control says not to show missings,
      ## remove Nmiss stat fun
      currstats <- control$numeric.stats
      currtest <- control$numeric.test
      vartype <- "numeric"
    }
    ############################################################

    ## if no missings, and control says not to show missings,
    ## remove Nmiss stat fun
    currstats <- if(length(attributes(currcol)$stats)>0) attributes(currcol)$stats else currstats
    if(!anyNA(currcol) && "Nmiss" %in% currstats) currstats <- currstats[currstats != "Nmiss"]
    for(statfun in currstats) {
      if(statfun %in% c("countrowpct", "countcellpct"))
      {
        bystatlist <- do.call(statfun, list(currcol, levels = xlevels,
                                            by = by.col, by.levels = by.levels, weights = weights, na.rm = TRUE))
      } else
      {
        for(bylev in by.levels) {
          idx <- by.col == bylev
          bystatlist[[as.character(bylev)]] <- do.call(statfun, list(currcol[idx], levels=xlevels, na.rm=TRUE, weights=weights[idx], ...))
        }
        ## add Total
        bystatlist$Total <- do.call(statfun, list(currcol, levels=xlevels, weights=weights, ...))
      }
      statList[[statfun]] <- bystatlist
    }

    currtest <- if(nchar(specialTests[eff]) > 0) specialTests[eff] else currtest
    testout <- if(control$test) {
      eval(call(currtest, currcol, by.col, chisq.correct=control$chisq.correct, simulate.p.value=control$simulate.p.value, B=control$B))
    } else NULL

    xList[[nameEff]] <- list(stats=statList, test=testout, label=labelEff, name=names(modeldf)[eff], type=vartype)
  }

  if(length(xList) == 0) stop("No x-variables successfully computed.")

  labelBy <- attributes(by.col)$label
  if(is.null(labelBy)) labelBy <- names(modeldf)[1]

  yList <- list()
  yList[[names(modeldf)[1]]] <- list(stats=c(unlist(table(factor(by.col, levels=by.levels), exclude=NA)), Total=sum(!is.na(by.col))),
                                     label=labelBy, name=names(modeldf)[1])

  structure(list(y = yList, x = xList, control = control, Call = match.call(), weights=userWeights), class = "tableby")
}


#' @rdname tableby
#' @export
print.tableby <- function(x, ...) {
  cat("Tableby Object\n\n")
  cat("Function Call: \n")
  print(x$Call)
  cat("\n")
  cat("y variable:\n")
  print(names(x$y))
  cat("x variables:\n")
  print(names(x$x))

  invisible(x)
}

