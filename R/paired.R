
#' Summary Statistics of a Set of Independent Variables Paired Across Two Timepoints
#'
#' Summarize one or more variables (x) by a paired time variable (y). Variables
#'   on the right side of the formula, i.e. independent variables, are summarized by the
#'   two time points on the left of the formula. Optionally, an appropriate test is performed to test the
#'   distribution of the independent variables across the time points.
#' @param formula an object of class \code{\link{formula}} of the form \code{time ~ var1 + ...}.
#'   See "Details" for more information.
#' @inheritParams tableby
#' @param id The vector giving IDs to match up data for the same subject across two timepoints.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s.
#'   The default is \code{na.paired("in.both")}. See \code{\link{na.paired}} for more details
#' @param control control parameters to handle optional settings within \code{paired}.
#'   Two aspects of \code{paired} are controlled with these: test options of RHS variables and x variable summaries.
#'   Arguments for \code{paired.control} can be passed to \code{paired} via the \code{...} argument,
#'   but if a control object and \code{...} arguments are both supplied,
#'   the latter are used. See \code{\link{paired.control}} for more details.
#' @param ... additional arguments to be passed to internal \code{paired} functions or \code{\link{paired.control}}.
#' @return An object with class \code{c("paired", "tableby")}
#' @details
#' Do note that this function piggybacks off of \code{\link{tableby}} quite heavily, so there is no
#' \code{summary.paired} function (for instance).
#'
#' These tests are accepted:
#' \itemize{
#'   \item{
#'     \code{paired.t}: a paired \code{\link[stats:t.test]{t-test}}.
#'   }
#'   \item{
#'     \code{mcnemar}: \link[stats:mcnemar.test]{McNemar's test}.
#'   }
#'   \item{
#'     \code{signed.rank}: a \link[stats:wilcox.test]{signed rank test}.
#'   }
#'   \item{
#'     \code{sign.test}: a sign test.
#'   }
#'   \item{
#'     \code{notest}: no test is performed.
#'   }
#' }
#' @seealso \code{\link{paired.control}}, \code{\link{tableby}}, \code{\link{formulize}}
#' @author Jason Sinnwell, Beth Atkinson, Ryan Lennon, and Ethan Heinzen
#' @name paired
NULL
#> NULL

#' @rdname paired
#' @export
paired <- function(formula, data, id, na.action, subset=NULL, strata, control = NULL, ...) {
  control <- c(list(...), control)
  control <- do.call("paired.control", control[!duplicated(names(control))])

  Call <- match.call()

  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula", "data", "na.action", "subset", "strata", "control", names(control), "id")
  match.idx <- match(names(Call)[-1], expectArgs)
  if(anyNA(match.idx)) warning("unused arguments: ", paste(names(Call)[1+which(is.na(match.idx))], collapse=", "), "\n")

  indx <- match(c("formula", "data", "subset", "na.action", "id", "strata"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required")
  if(length(formula) != 3) stop("'formula' must be two-sided.")
  if(indx[5] == 0) stop("An id argument is required")

  special <- c("paired.t", "mcnemar", "signed.rank", "sign.test", "notest")

  out.tables <- list()
  formula.list <- as_list_formula(formula)
  for(FORM in formula.list)
  {
    temp.call <- Call[c(1, indx)]
    temp.call[[1]] <- as.name("model.frame")

    if(is.null(temp.call$na.action)) temp.call$na.action <- na.paired("in.both")

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
      modeldf[[1]] <- NULL
    }

    if(length(by.levels) != 2) stop("Please specify exactly 2 time points")
    ids <- modeldf$`(id)`
    tab <- table(ids, by.col)
    if(sum(tab > 1) > 0)
      stop("At least one person has multiple observations for at least one time point")
    if(sum(rowSums(tab) == 2) == 0)
      stop("No one appears to have data on both time points")

    ids.both <- intersect(ids[by.col == by.levels[1]], ids[by.col == by.levels[2]])

    strata.col1 <- strata.col[by.col == by.levels[1]]
    TP1 <- modeldf[by.col == by.levels[1], , drop = FALSE]
    strata.col2 <- strata.col[by.col == by.levels[2]]
    TP2 <- modeldf[by.col == by.levels[2], , drop = FALSE]

    cn <- colnames(modeldf)
    cn <- cn[cn != "(id)"]

    idx1 <- match(ids.both, TP1$`(id)`, nomatch = 0)
    idx2 <- match(ids.both, TP2$`(id)`, nomatch = 0)
    strata.col1 <- strata.col1[idx1]
    strata.col2 <- strata.col2[idx2]
    TP1 <- TP1[idx1, cn, drop = FALSE]
    TP2 <- TP2[idx2, cn, drop = FALSE]
    modeldf[["(id)"]] <- NULL

    yList <- list(stats=c(table(factor(by.col, levels=by.levels), exclude=NA), Difference=length(ids.both)),
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

      for(eff in seq_along(modeldf)) {

        currcol <- modeldf[[eff]]
        TP1.eff <- TP1[[eff]]
        TP2.eff <- TP2[[eff]]

        ############################################################

        if(is.ordered(currcol) || is.logical(currcol) || is.factor(currcol) || is.character(currcol)) {
          ######## ordinal or categorical variable (character or factor) ###############

          ## convert logicals and characters to factor
          if(is.character(currcol))
          {
            lvl <- sort(unique(currcol[!is.na(currcol)]))
            currcol <- factor(currcol, levels = lvl)
            TP1.eff <- factor(TP1.eff, levels = lvl)
            TP2.eff <- factor(TP2.eff, levels = lvl)
          } else if(is.logical(currcol))
          {
            lvl <- c(FALSE, TRUE)
            currcol <- factor(currcol, levels=lvl)
            TP1.eff <- factor(TP1.eff, levels = lvl)
            TP2.eff <- factor(TP2.eff, levels = lvl)
          }


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
          stop("Sorry, survival objects don't work in this function.")

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
        TP1.eff <- TP1.eff[strata.col1 == strat]
        TP2.eff <- TP2.eff[strata.col2 == strat]
        if(!anyNA(currcol) && "Nmiss" %in% currstats) currstats <- currstats[currstats != "Nmiss"]
        statList <- list()
        for(statfun in currstats) {
          bystatlist <- list()
          if(statfun %in% c("countrowpct", "countcellpct", "rowbinomCI"))
          {
            bystatlist <- do.call(statfun, list(currcol, levels = xlevels,
                                                by = by.col, by.levels = by.levels, na.rm = TRUE))
            bystatlist$Total <- NULL
          } else
          {
            for(bylev in by.levels) {
              idx <- bycol == bylev
              bystatlist[[bylev]] <- do.call(statfun, list(currcol[idx], levels=xlevels, na.rm=TRUE, ...))
            }
          }
          if(statfun %in% c("countpct", "countrowpct", "countcellpct"))
          {
            # countrowpct to get the right percentages
            bystatlist$Difference <- countrowpct(TP1.eff, levels = xlevels, by = TP1.eff == TP2.eff,
                                                 by.levels = c(TRUE, FALSE), na.rm = TRUE)[[2]]
          } else if(statfun == "count")
          {
            # this doesn't have percentages
            bystatlist$Difference <- count(ifelse(TP1.eff == TP2.eff, TP1.eff, NA), levels = xlevels, na.rm = TRUE)
          } else if(statfun %in% c("binomCI", "rowbinomCI"))
          {
            bystatlist$Difference <- rowbinomCI(TP1.eff, levels = xlevels, by = TP1.eff == TP2.eff,
                                                by.levels = c(TRUE, FALSE), na.rm = TRUE)[[2]]
          } else
          {
            bystatlist$Difference <- do.call(statfun, list(as.numeric(TP2.eff) - as.numeric(TP1.eff),
                                                           levels=xlevels, na.rm=TRUE, ...))
          }
          statList[[statfun]] <- bystatlist
        }

        currtest <- if(nchar(specialTests[eff]) > 0) specialTests[eff] else currtest
        testout <- if(control$test) {
          eval(call(currtest, TP1.eff, TP2.eff, mcnemar.correct=control$mcnemar.correct,
                    signed.rank.exact = control$signed.rank.exact, signed.rank.correct = control$signed.rank.correct))
        } else NULL

        xList[[eff]] <- list(stats=statList, test=testout, type=vartype)
      }

      strataList[[if(strataTerm == "") 1 else paste0("(", strataTerm, ") == ", strat)]] <- xList
    }
    out.tables[[termBy]] <- list(formula = FORM, y = yList, strata = list(term = strataTerm, values = strata.levels, label = strataLabel),
                                 x = xTerms, tables = strataList, control.list = control.list)
  }
  structure(list(Call = Call, control = control, tables = out.tables, hasWeights = FALSE, hasStrata = hasStrata), class = c("paired", "tableby"))
}

#' @rdname paired
#' @export
print.paired <- function(x, ...)
{
  cat("Paired ")
  NextMethod()
  invisible(x)
}

