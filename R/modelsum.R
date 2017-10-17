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
#'   The default is set by the \code{na.modelsum} setting of \code{options}, and is \code{na.fail} if that is unset. The default is
#'   to include observations with \code{NA}s in x variables, but remove those with \code{NA} in response variable.
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
#' @author Jason Sinnwell, Patrick Votruba, Beth Atkinson, Gregory Dougherty, adapted from SAS Macro of the same name
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

modelsum <- function(formula,  family="gaussian", data, adjust=NULL, na.action=na.modelsum,
                     subset=NULL, weights=NULL, control = NULL, ...) {
  Call <- match.call()

  ## Allow family parameter to passed with or without quotes
  ##    exception is survival, would require public function named survival.
  ## Here, we force quotes to simplify in for loop below
  if (is.function(family))
    family <- family()$family

  if(family %nin% c("survival","gaussian","binomial","poisson","quasibinomial","quasipoisson"))
    stop("Family ", family, "not supported.\n")

  if(family != "survival" && any(grepl("Surv\\(", formula))) {
    warning("Found Surv in formula, assuming family='survival'\n")
    family <- "survival"
  }
  ## pick up extra control arguments from command via ...
  control <- c(list(...), control)
  control <- do.call("modelsum.control", control[!duplicated(names(control))])

  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula","family","adjust","data","na.action","subset","weights", "control", names(control))
  match.idx <- match(names(Call)[-1], expectArgs)
  if(any(is.na(match.idx))) {
    warning("unused arguments: ", paste(names(Call)[1+which(is.na(match.idx))],collapse=", "), "\n")
  }

  indx.adjust <- match(c("adjust"), names(Call), nomatch = 0)
  adjVars <- NULL

  if(indx.adjust != 0) {
    ## will add adjVars to end of RHS of formula
    j <- length(formula)
    adjLen <- length(adjust[[2]])
    if(adjLen < 2) {
      adjVars <- as.character(adjust[2])
    } else {
      adj2 <- as.list(adjust[[2]])
      while(adjLen >=2 & as.character(adj2[[1]]) %nin% c("pspline","offset")) {
        adjVars <- c(adjVars,as.character(adj2[length(adj2)]))
        adj2 <- as.list(adj2[-length(adj2)][[2]])
        adjLen <- length(adj2)
#        as.character(adjust[[2]][2:length(adjust[[2]])]))
      }
      adjVars <- c(adjVars,as.character(adj2)) # [2:length(adj2)]))
      adjVars <- adjVars[!(adjVars == "pspline" | adjVars == "offset")]
    }

    formula[[j]] <- call("+", formula[[j]], call("(", adjust[[2]]))
  }

  indx.subset <- match(c("subset"), names(Call), nomatch = 0)
  subsetVarsAdd <- NULL
  if(indx.subset != 0) {
    ## will add subsetVars to end of RHS of formula
    j <- length(formula)
     ## fix subsetting
    oldwarn <- options()$warn
    options(warn=-1)
    subsetVars <- unlist(stringr::str_match_all(as.character(Call[indx.subset]),names(data)))
    ##Subset to only those not already in formula
    subsetVarsAdd <- subsetVars[subsetVars %nin%
                       unlist(stringr::str_match_all(paste0(as.character(formula),collapse=""), subsetVars))]

    if(length(subsetVarsAdd)>0 ) {
      formula[[j]] <- call("+", formula[[j]], call("(",as.name(subsetVarsAdd)))
    }
    options(warn=oldwarn)
  }
#  indx <- match(c("formula", "data", "subset", "weights", "na.action"), names(Call), nomatch = 0)
#  if(indx[4] != 0) {   ## weights
#    weights <- as.vector(stats::model.weights(modeldf))

  temp.call <- call("model.frame", formula = formula)

  for (i in c("data", "subset", "weights", "na.action")) {
    if (!is.null(Call[[i]])) {
      temp.call[[i]] <- Call[[i]]
    }
  }
  if(is.null(temp.call$na.action)) {
    temp.call$na.action <- na.modelsum
  }
  if(!missing(data))
  {
    data <- keep.labels(data)
    # instead of call("keep.labels", ...), which breaks when arsenal isn't loaded (Can't find "keep.labels")
    temp.call$data <- as.call(list(keep.labels, temp.call$data))
  }

  ## if(is.null(temp.call$weights)) {
  ##    temp.call$weights <- rep(1, nrow())
  ##  }
  ## added 1/12/16 by JPS
  ## strip down formula for model.frame to just variables for use within loop
  baseFormula <- temp.call$formula
  ## y

  if(any(grepl("Surv",as.character((baseFormula[[2]]))))) {
    ## skip ybase, put something here   ##| length(baseFormula[[2]]) > 1
    ybase <- ""
  } else {
    ybase <- unlist(tapply(as.character(baseFormula[[2]]), 1:length(baseFormula[[2]]),
              function(x) {tmp <- stringr::str_match(x, colnames(data)); tmp[!is.na(tmp),1]}))
    ## if length > 1, choose one without parens "("
    ybase <- ybase[!grepl("\\(",ybase)]
    if(length(ybase)>1) {
      ## if still > 1, choose one that matches best
      ypct <- unlist(tapply(as.character(baseFormula[[2]]), 1:length(baseFormula[[2]]),
              function(x) {tmp <- stringr::str_match(x, colnames(data)); tmp <- tmp[!is.na(tmp),1]; nchar(tmp)/nchar(x)}))
      ybase <- ybase[which(abs(ypct-1) == min(abs(ypct-1)))]
    }
  }
  ## xvars + adjust
  xvars <- character()
  xformula <- as.character(baseFormula[[3]])
  for(xform in xformula) {
    if(xform == ".") {
      xvars <- colnames(data)[-grep(ybase,colnames(data))]
      next
    }
    if(xform %nin% c("+","*","|")) {
      xbase <- stringr::str_match(xform, colnames(data))
      xvars <- c(xvars, xbase[!is.na(xbase),1])
    }
  }

  base.call <- temp.call
  base.call$formula <- stats::as.formula(paste0(ybase, "~", paste(xvars, collapse="+")))
  ## undo for surv response, or I(fun(y))
  if(any(grepl("Surv",as.character((baseFormula[[2]]))))) {
  #if(any(grepl("Surv",as.character((baseFormula[[2]]))))) { #grepl("\\(",as.character(baseFormula[[2]])))) {
    base.call$formula[[3]] <- base.call$formula[[2]]
    base.call$formula[[2]] <- temp.call$formula[[2]]
  }

  ## create the environment where the formula will be evalulated
  tabenv <- new.env(parent = environment(formula))
  environment(temp.call$formula) <-  environment(base.call$formula) <- tabenv

  basedf <- eval.parent(base.call)
  modeldf <- eval.parent(temp.call)
  ## ----- add weights
  weights <- as.vector(stats::model.weights(modeldf))
  if(is.null(weights)) {
    weights <- rep(1, nrow(basedf))
    base.call$weights <- rep(1, nrow(basedf))
  }
  if (!is.null(weights) && (!is.numeric(weights) | any(weights<0))) {
    stop("'weights' must be a numeric vector and must be non-negative")
  }

  if("(weights)" %in% colnames(modeldf)) {
    modeldf <- modeldf[,!grepl("(weights)", colnames(modeldf))]
    basedf <- basedf[,!grepl("(weights)", colnames(basedf))]
  }

  ## assign weights, formula, data, etc. but don't need subset
  for(cc in names(base.call)[-c(1,grep("subset",names(base.call)))]){
    assign(cc, get(cc), envir=tabenv)
  }

  ## this assigns what is needed for evaluating all models with x, adjust, subset
  ##    check with: ls(envir=tabenv)
  assign("basedf",basedf, envir=tabenv)

  if(family=="survival" | family=="poisson") {
    ## put time/event/status vars into basedf, for surv, which is subsetted

    if(missing(subset)) {
      subset=rep(TRUE, nrow(data))
    } else {
      subset <- eval(base.call$subset, envir=data)
    }
    ## see if more rows were removed for NAs by na.action
    if(sum(subset) != nrow(basedf)) {
      subset[subset==TRUE & row.names(data) %nin% row.names(basedf)] <- FALSE
    }
    svars <- stringr::str_trim(strsplit(names(basedf)[1],split=",")[[1]],side="both")
    rparidx <- ifelse(grepl("\\)", svars), regexpr("\\)",svars)-1,nchar(svars))
    svars <- substr(svars, 1, rparidx)
    lparidx <- ifelse(grepl("\\(", svars), regexpr("\\(",svars)+1,1)
    svars <- substr(svars, lparidx,nchar(svars))
    for(sname in svars) {
      basedf[,sname] <- data[subset,sname]
    }
  }

  if (nrow(basedf) == 0) {
    stop("No (non-missing) observations")
  }

  fitList <- list()
  modeldf <- modeldf[,!duplicated(colnames(modeldf))]
  adjCols <- sapply(adjVars,function(x) grep(x, colnames(modeldf), fixed=TRUE))
  if(length(adjCols)<1) adjCols <- numeric()
  ##  adjCols <- which(colnames(modeldf) %in% adjVars)
  subsetCols <- which(colnames(modeldf) %in% subsetVarsAdd)
  ## effect columns are only those not in adjust and only in subset (excluded those in formula)
  effCols <- (1:ncol(modeldf))[-c(1,adjCols,subsetCols)]
  yTerm <- colnames(modeldf)[1]
  yLabel <- attributes(modeldf[,1])$label
  if(is.null(yLabel)) {
    yLabel <- yTerm
  }

  for(eff in effCols) {

    formulaStr <- paste0(yTerm, "~", paste(colnames(modeldf)[c(eff, adjCols)], collapse="+"))

    ## placeholder for ordered, don't do any fitting
    ## y is ordered factor
    if (family == "ordered") {
      xname <- colnames(modeldf)[eff]
      ## look into using same ordered test from tableby
      fitList[[xname]] <- list(#coeff=summary(coeff(p(modeldf[,1]~ modeldf[,eff]),
                               N=sum(!is.na(modeldf[,eff])),
                               family="ordered", label=xname)
    } else if (family == "gaussian") {
      ## issue warning if appears categorical
      if(length(unique(modeldf[,1])) <= 5) {
        warning("Input family=gaussian, but dependent variable has 5 or fewer categories\n")
      }
      xname <- colnames(modeldf)[eff]
      labelEff <-  attributes(modeldf[,eff])$label
      if(is.null(labelEff))  labelEff <- xname

      lmfit <- eval(call("lm", formula=formulaStr, data=basedf, x=TRUE, weights=weights), envir=tabenv)
  ## lmfit <- stats::lm(formulaStr, data=basedf, weights="weights", )
      coeffTidy <- broom::tidy(lmfit, conf.int=TRUE, conf.level=control$conf.level)

      if(any(grepl("(weights)", colnames(lmfit$model)))) {
        lmfit$model <- lmfit$model[,-grep("(weights)", colnames(lmfit$model))]
      }
      coeffTidy$standard.estimate <- lm.beta(lmfit)
      names(coeffTidy) <- gsub("conf.low","CI.lower.estimate",
                            gsub("conf.high", "CI.upper.estimate",names(coeffTidy)))
      xterms=coeffTidy[grep(xname,coeffTidy$term,fixed=TRUE),"term"]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname)) {
        labelEff <- gsub(xname, paste0(labelEff, " "), xterms)
      }
      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy[grep(adj,coeffTidy$term,fixed=TRUE),"term"]
        if(length(aterm)>0) {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(modeldf[,adj])$label
          if(is.null(alabel))
            alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj))
            alabel <- gsub(adj, paste0(alabel, " "), aterm)
          adjlabels <- c(adjlabels, alabel)
        }
      }

      ## Continuous variable (numeric) ###############
      ## Note: Using tidy changes colname from 't value' to 'statistic'
      modelGlance <- c(broom::glance(lmfit),N=sum(!is.na(modeldf[,eff])),
                               Nmiss=sum(is.na(modeldf[,eff])))
      names(modelGlance) <- gsub("p.value","p.value.F", names(modelGlance))
      if(any(grepl("Nmiss2",control$gaussian.stats))) {
        names(modelGlance) <- gsub("Nmiss","Nmiss2", names(modelGlance))
      }
      fitList[[xname]] <- list(coeff=coeffTidy,
                               family="gaussian",
                               xterms=xterms, label=labelEff,
                               adjterms=adjterms, adjlabels=adjlabels,
                               glance=modelGlance)

    } else if (family == "binomial" || family == "quasibinomial") {
      ## These families are used in glm

      ## I think this is taken care of with the pROC:: below
      ## require(pROC, quietly=TRUE,warn.conflicts=FALSE)

      xname <- colnames(modeldf)[eff]
      labelEff <-  attributes(modeldf[,eff])$label
      if(is.null(labelEff))  labelEff <- xname

      fit <- eval(call("glm", formula=formulaStr, data=basedf, family=family, x=TRUE, weights=weights), envir=tabenv)
     # fit <- glm(formulaStr, data=basedf, family=family, x=TRUE, weights="weights")
      rocOut <- pROC::roc(fit$y ~ predict(fit, type='response'))
      #coeffbeta <- summary(fit)$coef
      ## find out that broom:::tidy.lm allows conf.int and exp
      coeffORTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
      coeffORTidy[grep("Intercept",coeffORTidy$term),-1] <- NA
      coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)
      names(coeffTidy) <- gsub("conf.low","CI.lower.estimate",
                               gsub("conf.high", "CI.upper.estimate",names(coeffTidy)))

      coeffTidy <- data.frame(coeffTidy, OR=coeffORTidy$estimate,
                              CI.lower.OR=coeffORTidy$conf.low,
                              CI.upper.OR=coeffORTidy$conf.high)

      xterms <- coeffTidy[grep(xname,coeffTidy$term,fixed=TRUE),"term"]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname)) {
        labelEff <- gsub(xname, paste0(labelEff, " "), xterms)
      }
      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy[grep(adj,coeffTidy$term,fixed=TRUE),"term"]
        if(length(aterm)>0) {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(modeldf[,adj])$label
          if(is.null(alabel))
            alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj))
            alabel <- gsub(adj, paste0(alabel, " "), aterm)
          adjlabels <- c(adjlabels, alabel)
        }
      }
      ## tidy data frame has extra column for terms (row names), shift col index +1
      ## 'z value' changed to 'statistic'
      modelGlance <- c(broom::glance(fit),concordance=pROC::auc(rocOut),
                       N=sum(!is.na(modeldf[,eff])), Nmiss=sum(is.na(modeldf[,eff])))
      if(any(grepl("Nmiss2",control$binomial.stats))) {
        names(modelGlance) <- gsub("Nmiss","Nmiss2", names(modelGlance))
      }
      fitList[[xname]] <- list(coeff=coeffTidy,
                               family=family,
                               xterms=xterms, label=labelEff,
                               adjterms=adjterms, adjlabels=adjlabels,
                               glance=modelGlance)

    } else if (family == "quasipoisson" || family == "poisson") {
      ## These families use glm
      xname <- colnames(modeldf)[eff]
      labelEff <-  attributes(modeldf[,eff])$label
      if(is.null(labelEff))  labelEff <- xname
      fit <- eval(call("glm", formula=formulaStr, data=basedf, family=family, x=TRUE, weights=weights), envir=tabenv)

##      fit <- glm(formulaStr, data=basedf, family=family, x=TRUE, weights=weights)
      #coeffbeta <- summary(fit)$coef
      ## find out that broom:::tidy.lm allows conf.int and exp
      coeffRRTidy <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE, conf.level=control$conf.level)
      coeffRRTidy[grep("Intercept",coeffRRTidy$term),-1] <- NA
      coeffTidy <- broom::tidy(fit, exponentiate=FALSE, conf.int=TRUE, conf.level=control$conf.level)
      names(coeffTidy) <- gsub("conf.low","CI.lower.estimate",
                            gsub("conf.high", "CI.upper.estimate",names(coeffTidy)))

      coeffTidy <- data.frame(coeffTidy, RR=coeffRRTidy$estimate,
                              CI.lower.RR=coeffRRTidy$conf.low,
                              CI.upper.RR=coeffRRTidy$conf.high)

      xterms <- coeffTidy[grep(xname,coeffTidy$term,fixed=TRUE),"term"]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname)) {
        labelEff <- gsub(xname, paste0(labelEff, " "), xterms)
      }
      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy[grep(adj,coeffTidy$term,fixed=TRUE),"term"]
        if(length(aterm)>0) {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(modeldf[,adj])$label
          if(is.null(alabel))
            alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj))
            alabel <- gsub(adj, paste0(alabel, " "), aterm)
          adjlabels <- c(adjlabels, alabel)
        }
      }
      ## tidy data frame has extra column for terms (row names), shift col index +1
      ## 'z value' changed to 'statistic'

      modelGlance <- c(broom::glance(fit),N=sum(!is.na(modeldf[,eff])), Nmiss=sum(is.na(modeldf[,eff])))
      if(any(grepl("Nmiss2",control$poisson.stats))) {
        names(modelGlance) <- gsub("Nmiss","Nmiss2", names(modelGlance))
      }
      fitList[[xname]] <- list(coeff=coeffTidy,
                            family=family,
                            xterms=xterms, label=labelEff,
                            adjterms=adjterms, adjlabels=adjlabels,
                            glance=modelGlance)

    } else if(family=="survival") {

      xname <- colnames(modeldf)[eff]
      labelEff <-  attributes(modeldf[,eff])$label
      if(is.null(labelEff))  labelEff <- xname

      ph <- eval(call("coxph", formula=stats::as.formula(formulaStr), data=basedf, weights=weights), envir=tabenv)

      #ph <- coxph(stats::as.formula(formulaStr), data=data, weights=weights) ## should be this:modeldf)
      ## use tidy to get both CIs, merge
      coeffHRTidy <- broom::tidy(ph, exponentiate=TRUE, conf.int=.95)
      coeffTidy <- broom::tidy(ph, exponentiate=FALSE, conf.int=.95)
      names(coeffTidy) <- gsub("conf.low","CI.lower.estimate",
                            gsub("conf.high", "CI.upper.estimate",names(coeffTidy)))
      coeffTidy <- data.frame(coeffTidy, HR=coeffHRTidy$estimate,
                              CI.lower.HR=coeffHRTidy$conf.low,
                              CI.upper.HR=coeffHRTidy$conf.high)

 ##     coeffTidy[ coeffTidy > 1e60  | coeffTidy < -1e60] <- NA

      xterms <- coeffTidy[grep(xname,coeffTidy$term,fixed=TRUE),"term"]
      ## handle when xterm is categorical with level tagged on
      if(nchar(xterms[1]) > nchar(xname)) {
        labelEff <- gsub(xname, paste0(labelEff, " "), xterms)
      }
      adjterms <- adjlabels <- NULL
      for(adj in adjVars) { ## manage adj terms and labels
        aterm <- coeffTidy[grep(adj,coeffTidy$term,fixed=TRUE),"term"]
        if(length(aterm)>0) {
          adjterms <- c(adjterms, aterm)
          alabel <- attributes(modeldf[,adj])$label
          if(is.null(alabel))
            alabel <- adj
          ## handle when adj term is categorical with level tagged on
          if(nchar(aterm[1]) > nchar(adj))
            alabel <- gsub(adj, paste0(alabel, " "), aterm)
          adjlabels <- c(adjlabels, alabel)
        }
      }

      ## work with fit to get hr, try summary(fit) as above
      modelGlance <-  c(broom::glance(ph),Nmiss=sum(is.na(modeldf[,eff])))
      names(modelGlance) <- gsub("n$","N", gsub("nevent","Nevent", names(modelGlance)))
      if(any(grepl("Nmiss2",control$survival.stats))) {
        names(modelGlance) <- gsub("Nmiss","Nmiss2", names(modelGlance))
      }
      ## Survival (time to event) #######
      fitList[[xname]] <- list(coeff=coeffTidy,
                           family="survival",
                           xterms=xterms, label=labelEff,
                           adjterms=adjterms, adjlabels=adjlabels,
                           glance=c(broom::glance(ph),
                           N=sum(!is.na(modeldf[,eff])),Nmiss=sum(is.na(modeldf[,eff]))))
    }
## put xname and endpoint in glance, summary and as.data.frame to pull from there
    fitList[[xname]]$glance <- c(fitList[[xname]]$glance, endpoint=yTerm, endlabel=yLabel, x=xname)

  } # end for: eff

  msList <- list(fits=fitList, control = control, Call = Call, family=family)
                 ##, adjust=adjVars)
  class(msList) <- "modelsum"
  return(msList)
}

## Needed for being able to use "survival" with or without quotes,
##   keep as private function
survival <- function() list(family="survival")


mySeq <- function(from, to) {
	if (from > to)
		return(seq_len(0))

	return(seq(from, to))
}


#' @rdname modelsum
#' @export
print.modelsum <- function(x, ...) {
#  if (x$family == "gaussian") {
#    printGaussian(x)
#  }

#  else {
    cat("Modelsum S3 Object\n\n")
    cat("Function Call: \n")
    print(x$Call)
    cat("\n")
    cat("y variable:\n")
    print(x$fits[[1]]$y)
    cat("x variables:\n")
    xvars <- NULL
    for (ii in 1:length(x$fits)) {
      xvars <- c(xvars, x$fits[[ii]]$x)
    }
    print(xvars)
#  }
  invisible(x)
}

#' @rdname modelsum
#' @export
print.modelsumList <- function(x, ...) {
  lapply(x, print.modelsum, ...)
  invisible(x)
}
