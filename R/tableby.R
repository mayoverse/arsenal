## Purpose: create analysis results from a formula, summarizing the response
## by the RHS variables, which univariate stats on the RHS vars within the
## levels of the response
## Author: Jason Sinnwell and Beth Atkinson
## Updated: 9/29/2015
##
## to work with "specials" to specify the type of test or variable it is.
## look at survival package:


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
#'   typically the environment from which \code{tableby} is called.
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
#' @param ... additional arguments to be passed to internal \code{tableby} functions. See "Details" for information.
#'   Currently not implemented in \code{print.tableby}.
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
#'     \code{logrank}: log-rank , the default for time-to-event variables
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
#' @return
#'
#' An object with class \code{'tableby'}, which is effectively a list with
#' the variables from the right-side in x and the group variable in y (if any).
#' Then, each item in x has these:
#'
#' \item{stats}{Summary statistics of the RHS variable within each level of the LHS variable}
#' \item{test}{Formal test of the distribution of the RHS variable across the levels of the LHS variable}
#' \item{label}{The label attribute of a variable. It is set to the label attribute of a data column, if it exists,
#'   otherwise set to the variable name in \code{data}. Can be changed with \code{\link{labels.tableby}} function for the tableby object.}
#'
#' The object also contains the original function call and the \code{tableby.control} list that is used in \code{tableby}.
#'
#' @seealso \code{\link[stats]{anova}}, \code{\link[stats]{chisq.test}}, \code{\link{tableby.control}},
#'   \code{\link{print.tableby}}, \code{\link{summary.tableby}}, \code{\link{formulize}}
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
#' @author Jason Sinnwell, Beth Atkinson, Gregory Dougherty, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
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
  expectArgs <- c("formula","data","na.action","subset","weights", "control", names(control))
  match.idx <- match(names(Call)[-1], expectArgs)
  if(any(is.na(match.idx))) {
    warning("unused arguments: ", paste(names(Call)[1+which(is.na(match.idx))],collapse=", "), "\n")
  }
  ## pick up extra control arguments from command via ...
  control <- do.call("tableby.control", control)

  indx <- match(c("formula", "data", "subset", "weights", "na.action"), names(Call), nomatch = 0)
  if(indx[1] == 0) {   ## formula
    stop("A formula argument is required")
  }
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
    temp.call$data <- call("keep.labels", temp.call$data)
    temp.call$formula <- stats::terms(formula, special, data = keep.labels(data))
  }
  ##  set up new environment for
  ## if specials, assign dummy versions of those functions
     ## if(any(!is.null(attr(temp.call$formula, "specials"))))
  tabenv <- new.env(parent = environment(formula))

  if (!is.null(attr(temp.call$formula, "specials")$anova)) {
    ## allow stat functions to be passed as single arguments that are strings of function names
    ## Store this as attribute in the modeldf column, along with the actual name of the variable,
    ## rather than anova(age) showing up in the result (though anova(age) will be the column name in modeldf
    ## but we pull these attributes off later.
    assign("anova", function(x, ...)
           { extraArgs <- list(...); attr(x, "name") <- deparse(substitute(x)); attr(x, "stats") <- extraArgs; x},
           envir = tabenv)
  }
  if (!is.null(attr(temp.call$formula, "specials")$chisq)) {
    assign("chisq", function(x, ...)
           { extraArgs <- list(...); attr(x, "name") <- deparse(substitute(x)); attr(x, "stats") <- extraArgs; x},
           envir = tabenv)
  }
  if (!is.null(attr(temp.call$formula, "specials")$trend)) {
    assign("trend", function(x, ...)
           { extraArgs <- list(...); attr(x, "name") <- deparse(substitute(x)); attr(x, "stats") <- extraArgs; x},
           envir = tabenv)
  }
  if (!is.null(attr(temp.call$formula, "specials")$kwt)) {
    assign("kwt", function(x, ...)
           { extraArgs <- list(...); attr(x, "name") <- deparse(substitute(x)); attr(x, "stats") <- extraArgs; x},
           envir = tabenv)
  }
  if (!is.null(attr(temp.call$formula, "specials")$fe)) {
    assign("fe", function(x, ...)
           { extraArgs <- list(...); attr(x, "name") <- deparse(substitute(x)); attr(x, "stats") <- extraArgs; x},
           envir = tabenv)
  }
  if (!is.null(attr(temp.call$formula, "specials")$logrank)) {
    assign("logrank", function(x, ...)
           { extraArgs <- list(...); attr(x, "name") <- deparse(substitute(x)); attr(x, "stats") <- extraArgs; x},
           envir = tabenv)
  }
  ## set tabenv as environment in which to evalulate formula
  #if(any(!is.null(attr(temp.call$formula, "specials"))))
  environment(temp.call$formula) <- tabenv

  ## evaluate the formula with env set for it
  modeldf <- eval.parent(temp.call)
  if (nrow(modeldf) == 0) {
    stop("No (non-missing) observations")
  }
  Terms <- stats::terms(modeldf)

  if(attributes(Terms)$response == 0) {
    ## no response, create a dummy one
    modeldf <- data.frame(Total="Overall",modeldf, stringsAsFactors=FALSE)
    control$total <- FALSE
    control$test <- FALSE
  }
  weights <- as.vector(stats::model.weights(modeldf))
  if(is.null(weights)) {
    weights <- rep(1, nrow(modeldf))
    userWeights=FALSE
  }
  if("(weights)" %in% colnames(modeldf)) {
    modeldf <- modeldf[,!grepl("(weights)", colnames(modeldf))]
    userWeights=TRUE
  }
  if (!is.null(weights) && (!is.numeric(weights) | any(weights<0))) {
    stop("'weights' must be a numeric vector and must be non-negative")
  }

  ## find which columnss of modeldf have specials assigned to known specials
  specialIndices <- unlist(attr(Terms, "specials"))
  specialTests <- rep("",ncol(modeldf))
  ## If a special shows up multiple times, the unlist assigned a number at the end. Strip it off.
  ## This disallows functions with a number at the end, and trims off up to 999 instances of
  ## the same test name
  specialTests[specialIndices] <- gsub("[0-9]$","",gsub("[0-9]$","",gsub("[0-9]$","",names(specialIndices))))

  ## list of x variables
  xList <- list()

  ## fix of droplevels on by factor suggested by Ethan Heinzen 4/12/2016
  if(is.factor(modeldf[,1])) {
    modeldf[,1] <- droplevels(modeldf[,1])
  }

  for(eff in 2:ncol(modeldf)) {

    currcol <- modeldf[,eff]

    ## label
    nameEff <- attributes(currcol)$name
    if(is.null(nameEff))  nameEff <- names(modeldf)[eff]
    labelEff <-  attributes(currcol)$label
    if(is.null(labelEff))  labelEff <- nameEff

    ## ordered factor
    if(is.ordered(currcol)) {

      ## stats
      ostatList <- list()
      ostyles <- character()
      xlevels <- levels(currcol)
      if(length(xlevels) == 0)
      {
        warning(paste0("Zero-length levels found for ", nameEff))
        next
      }

      ## get stats funs from either formula  or control
      ordered.stats <- if(length(attributes(currcol)$stats)>0) {
        attributes(currcol)$stats
      } else {
        control$ordered.stats
      }
      ## if no missings, and control says not to show missings,
      ## remove Nmiss stat fun
      if(sum(is.na(currcol)) == 0 && any(grepl("Nmiss$",ordered.stats))) {
        ordered.stats <- ordered.stats[!grepl("Nmiss$", ordered.stats)]
      }
      for(statfun in ordered.stats) {
        ostyles <- c(ostyles, ifelse(statfun %in% c("countpct"), "percent",NA))

        bystatlist <- list()
        for(bylev in sort(unique(modeldf[,1]))) {
          idx <- which(modeldf[,1] == bylev)
          bystatlist[[as.character(bylev)]] <- eval(call(statfun, modeldf[idx,eff], levels=xlevels, na.rm=TRUE, weights=weights[idx]))
        }
        ostatList[[statfun]] <- bystatlist

        ## add Total
        if(control$total) {
          ostatList[[statfun]]$Total <- eval(call(statfun,currcol, levels=xlevels, weights=weights))
        }
      }

      ## test
      if(control$test) {
        if(nchar(specialTests[eff]) > 0) {
          testout <- eval(call(specialTests[eff], currcol, modeldf[,1]))
        } else {
          testout <- eval(call(control$ordered.test, currcol, modeldf[,1]))
        }
      } else {
        testout <- NULL
      }

      xList[[nameEff]] <- list(stats=ostatList, test=testout, label=labelEff,
                                           name=names(modeldf)[eff],
                                           type="ordinal", output=ostyles)

    } else if(is.logical(currcol) || is.factor(currcol) || is.character(currcol)) {
    ##############################################
    ## categorical variable (character or factor)
    ##############################################

      ## stats
      cstatList <- list()
      cstyles <- character()

      ## convert logicals to factor
      if(is.logical(currcol)) {
        currcol<- factor(currcol, levels=c(FALSE, TRUE))
      }

      ## to make sure all levels of cat variable are counted, need to pass values along
      xlevels <- if(is.factor(currcol)) {
        levels(currcol)
      } else {
        sort(unique(currcol[!is.na(currcol)]))
      }
      if(length(xlevels) == 0)
      {
        warning(paste0("Zero-length levels found for ", nameEff))
        next
      }

      ## if no missings, and control says not to show missings,
      ## remove Nmiss stat fun
      cat.stats <- if(length(attributes(modeldf[,2])$stats)>0) {
        attributes(currcol)$stats
      } else {
        control$cat.stats
      }
      if(sum(is.na(currcol)) == 0 && any(grepl("Nmiss$",cat.stats))) {
        cat.stats <- cat.stats[!grepl("Nmiss$", cat.stats)]
      }
      for(statfun in cat.stats) {
        cstyles <- c(cstyles, ifelse(statfun %in% c("countpct"), "percent",NA))
        bystatlist <- list()
        for(bylev in sort(unique(modeldf[,1]))) {
          idx <- which(modeldf[,1] == bylev)
          bystatlist[[as.character(bylev)]] <- eval(call(statfun, modeldf[idx,eff], levels=xlevels, na.rm=TRUE, weights=weights[idx]))
        }
        cstatList[[statfun]] <- bystatlist
        ## without weights can do:
        ##   tapply(currcol,modeldf[,1], statfun, levels=xlevels, na.rm=TRUE,simplify=FALSE, weights=weights, ...)

        ## add Total
        if(control$total) {
          cstatList[[statfun]]$Total <- eval(call(statfun,currcol, levels=xlevels, weights=weights))
        }
      }

      ## simplify, only do if num-levels is 2
      if(control$cat.simplify==TRUE &
         !is.null(nrow(cstatList[[statfun]][[1]])) && nrow(cstatList[[statfun]][[1]])==2) {
        ##length(xlevels)==2) {
        cstatList[[statfun]] <- lapply(cstatList[[statfun]], function(x) x[-1,])
      }

      ## test
      if(control$test) {
        if(nchar(specialTests[eff]) > 0) {
          testout <- eval(call(specialTests[eff], currcol, modeldf[,1]))
        } else {
          testout <- eval(call(control$cat.test, currcol, modeldf[,1]))
        }
      } else {
        testout <- NULL
      }

      xList[[nameEff]] <- list(stats=cstatList, test=testout, label=labelEff,
                               name=names(modeldf)[eff],
                               type="categorical", output=cstyles)

    } else if(is.Date(currcol)) {

      ######## Date variable ###############

      #stats
      dstatList <- list()
      dstyles <- character()
      ## if no missings, and control says not to show missings,
      ## remove Nmiss stat fun
      date.stats <-  if(length(attributes(currcol)$stats)>0) {
        attributes(currcol)$stats
      } else {
        control$date.stats
      }
      if(sum(is.na(currcol)) == 0 && any(grepl("Nmiss$",date.stats))) {
        date.stats <- date.stats[!grepl("Nmiss$", date.stats)]
      }

      for(statfun in date.stats) {
        dstyles <- c(dstyles, ifelse(statfun %in% "range", "range",
                        ifelse(statfun %in% "q1q3","list",
                           ifelse(statfun %in% c("medianrange","medianq1q3"), "medlist",NA))))

        bystatlist <- list()
        for(bylev in sort(unique(modeldf[,1]))) {
          idx <- which(modeldf[,1] == bylev)
          bystatlist[[as.character(bylev)]] <- eval(call(statfun, modeldf[idx,eff], na.rm=TRUE, weights=weights[idx]))
        }
        dstatList[[statfun]] <- bystatlist
        ## this works for median(date), but the above gets bad bc of list
        ##  dstatList[[statfun]] <- lapply(as.list(as.integer(currcol), modeldf[,1], statfun, na.rm=TRUE),
        ##      as.Date, origin="1970/01/01")
        ## add Total
        if(control$total) {
          dstatList[[statfun]]$Total <- eval(call(statfun,currcol, na.rm=TRUE, weights=weights))
        }
      }

      ## tests: kruskal.test
      if(control$test) {
        if(nchar(specialTests[eff]) > 0) {
          testout <- eval(call(specialTests[eff], currcol, modeldf[,1]))
        } else {
          testout <- eval(call(control$date.test, currcol, modeldf[,1]))
        }
      } else {
        testout <- NULL
      }

      xList[[nameEff]] <- list(stats=dstatList, test=testout, label=labelEff,
                                           name=names(modeldf)[eff],
                                           type="Date", output=dstyles)

    } else if(survival::is.Surv(currcol)) {

      ##### Survival (time to event) #######

      ## stats
      sstatList <- stimestatList <- list()
      sstyles <- stimestyles <- character()  ## pass times to summary, delay to within loop
      times <- list(...)$times
      if(is.null(times)) {
        times <- 1:5
      }
      stratfit <- survival::survfit(currcol ~ modeldf[,1], weights=weights)
      totfit <- survival::survfit(currcol ~ 1, weights=weights)
      for(statfun in control$surv.stats) {
        sstyles <- c(sstyles, ifelse(statfun=="NeventsSurv", "pct", NA))
        sstatList[[statfun]] <- as.list(eval(call(statfun, stratfit, times=times)))
        ## add Total
        if(control$total) {
          sstatList[[statfun]]$Total <- eval(call(statfun,totfit, times=times))
        }
      }

      ## test
      if(control$test) {
        if(nchar(specialTests[eff]) > 0) {
          testout <- eval(call(specialTests[eff], currcol, modeldf[,1]))
        } else {
          testout <- eval(call(control$surv.test, currcol, modeldf[,1]))
        }
      } else {
        testout <- NULL
      }

      ## if statfuns contain NeventsSurv or NriskSurv, make new surv x variable
      ## and rm from ssstatList and sstyles
      evsurv <- risksurv <- FALSE
      if("NeventsSurv" %in% names(sstatList)) {
        idx <- which(names(sstatList) == "NeventsSurv")
        sevstatList <- sstatList[[idx]]
        sevstyles <- sstyles[idx]
        sstatList[[idx]] <- NULL
        sstyles <- sstyles[-idx]
        evsurv <- TRUE
      }
       if("NriskSurv" %in% names(sstatList) ) {
        idx <- which(names(sstatList) == "NriskSurv")
        srskstatList <- sstatList[[idx]]
        srskstyles <- sstyles[idx]
        sstatList[[idx]] <- NULL
        sstyles <- sstyles[-idx]
        risksurv <- TRUE
      }

      xList[[nameEff]] <- list(stats=sstatList, test=testout, label=labelEff,
                                           name=names(modeldf)[eff],
                                           type="survival", output=sstyles)
      if(evsurv) {
        xList[["NeventsSurv"]] <- list(stats=list(NeventsSurv=sevstatList), test=testout, label="NeventsSurv",
                                           name=gsub("Surv", "Events", names(modeldf)[eff]),
                                           type="survival", output=sevstyles)
      }
      if(risksurv) {
         xList[["NriskSurv"]] <- list(stats=list(NriskSurv=srskstatList), test=testout, label="NriskSurv",
                                           name=gsub("Surv","AtRisk",names(modeldf)[eff]),
                                           type="survival", output=srskstyles)
      }

    } else if(is.numeric(currcol) || inherits(currcol, "difftime")) {

      ######## Continuous variable (numeric) ###############

      #stats
      nstatList <- list()
      nstyles <- character()

      ## for difftime, convert to numeric
      if(inherits(currcol, "difftime")) {
        currcol <- as.numeric(currcol)
      }

      ## if no missings, and control says not to show missings,
      ## remove Nmiss stat fun
      num.stats <-  if(length(attributes(currcol)$stats)>0) {
        attributes(currcol)$stats
      } else {
        control$numeric.stats
      }
      if(sum(is.na(currcol)) == 0 && any(grepl("Nmiss$",num.stats))) {
        num.stats <- num.stats[!grepl("Nmiss$", num.stats)]
      }
      for(statfun in num.stats) {
        nstyles <- c(nstyles, ifelse(statfun %in% "range", "range",
                                 ifelse(statfun %in% "q1q3","list",
                                   ifelse(statfun %in% c("medianrange","medianq1q3"), "medlist",NA))))
        bystatlist <- list()
        for(bylev in sort(unique(modeldf[,1]))) {
          idx <- which(modeldf[,1] == bylev)
          bystatlist[[as.character(bylev)]] <- eval(call(statfun, modeldf[idx,eff], na.rm=TRUE, weights=weights[idx]))
        }
        nstatList[[statfun]] <- bystatlist
        ## add Total
        if(control$total) {
          nstatList[[statfun]]$Total <- eval(call(statfun,currcol, na.rm=TRUE, weights=weights))
        }
        ## old way to call with ind_var, group_var, may go back, so keep around:
        ##  nstatList[[statfun]] <- eval(call(statfun, currcol, modeldf[,1]))
      }
      ## tests: anova and kruskal.test
      if(control$test) {
        if(nchar(specialTests[eff]) > 0) {
          testout <- eval(call(specialTests[eff], currcol, modeldf[,1]))
        } else {
          testout <- eval(call(control$numeric.test, currcol, modeldf[,1]))
        }
      } else {
        testout <- NULL
      }

      xList[[nameEff]] <- list(stats=nstatList, test=testout, label=labelEff,
                                           name=names(modeldf)[eff],
                                           type="numeric", output=nstyles)

    }
  }

  if(length(xList) == 0) stop("No x-variables successfully computed.")

  ## attributes: label/long-names
  ## number of RHS variables

  labelBy <- attributes(modeldf[,1])$label
  if(is.null(labelBy)) {
    labelBy <- names(modeldf)[1]
  }
  yList <- list()

  yList[[names(modeldf)[1]]] <- list(stats=unlist(table(factor(modeldf[,1],
                                     levels=sort(unique(modeldf[,1]))),exclude=NA)),
                                     label=labelBy, name=names(modeldf)[1])

  if(control$total) {
     yList[[names(modeldf)[1]]]$stats <- c(yList[[names(modeldf)[1]]]$stats,Total=sum(!is.na(modeldf[,1])))
  }

  tblList <- list(y = yList, x = xList, control = control, Call = match.call(), weights=userWeights)
  class(tblList) <- "tableby"
  return(tblList)
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

