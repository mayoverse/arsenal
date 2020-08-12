
#' Fit models over each of a set of independent variables with a response variable
#'
#' Fit and summarize models for each independent (x) variable with a response variable (y), with options to adjust by variables for each model.
#'
#' @param formula an object of class \code{\link{formula}}; a symbolic description of the variables to be modeled.  See "Details" for more information.
#' @param adjust an object of class \code{\link{formula}} or a list of formulas, listing variables to adjust by in all models.
#'  Specify as a one-sided formula, like: \code{~Age+ Sex}. If a list, the names are used for the summary function. Unadjusted models
#'  can be specified as \code{~ 1} or as a list: \code{list(Unadjusted = NULL)}.
#' @param family similar mechanism to \code{\link[stats]{glm}}, where the model to be fit is driven by the family.
#'   Options include: binomial, gaussian, survival, poisson, negbin, clog, and ordinal. These can be passed as a string, as a function,
#'   or as a list resulting from a call to one of the functions. See \code{\link{modelsum.family}} for details on
#'   survival, ordinal, negbin, and clog families.
#' @param data an optional data.frame, list or environment (or object coercible by \code{\link[base]{as.data.frame}} to a data frame) containing the
#'   variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically
#'   the environment from which \code{modelsum} is called.
#' @param subset an optional vector specifying a subset of observations (rows of \code{data}) to be used in the results.
#'   If \code{strata} is missing, this works as vector of logicals or an index; otherwise, it should be a logical vector.
#' @param weights an optional vector specifying the weights to apply to each data observation (rows of \code{data})
#' @param strata a vector of strata to separate model summaries by an additional group. Note that for families like "clog",
#'   the "usual" strata term to indicate subject groupings should be given in the \code{adjust} argument.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s.
#'   The default (\code{NULL}) is to use the defaults of \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, or \code{\link[survival]{coxph}},
#'   depending on the \code{family} specifications.
#' @param id A vector to identify clusters. Only used for \code{\link{relrisk}} at this time.
#' @param control control parameters to handle optional settings within \code{modelsum}.  Arguments for \code{modelsum.control}
#'   can be passed to \code{modelsum} via the \code{...} argument, but if a control object and \code{...} arguments are both supplied,
#'   the latter are used. See \code{\link{modelsum.control}} for other details.
#' @param ... additional arguments to be passed to internal \code{modelsum} functions.
#' @return An object with class \code{c("modelsum", "arsenal_table")}
#' @author Jason Sinnwell, Patrick Votruba, Beth Atkinson, Gregory Dougherty, and Ethan Heinzen, adapted from SAS Macro of the same name
#' @seealso \code{\link{arsenal_table}}, \code{\link{modelsum.control}}, \code{\link{summary.modelsum}},
#'   \code{\link{modelsum.internal}}, \code{\link{formulize}}
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
                     subset=NULL, weights=NULL, id, strata, control = NULL, ...) {
  Call <- match.call()

  ## Allow family parameter to passed with or without quotes
  ## Here, we force quotes to simplify in for loop below
  family.list <- if(is.function(family) || is.character(family)) match.fun(family)() else family

  if(family.list$family %nin% c("survival", "gaussian", "binomial", "poisson", "quasibinomial", "quasipoisson", "ordinal", "negbin", "clog", "relrisk"))
    stop("Family ", family.list$family, " not supported.\n")

  if(family.list$family != "survival" && any(grepl("Surv\\(", formula))) {
    warning("Found Surv in formula, assuming family='survival'\n")
    family.list <- survival()
  }
  ## pick up extra control arguments from command via ...
  control <- c(list(...), control)
  control <- do.call("modelsum.control", control[!duplicated(names(control))])

  ## Tell user if they passed an argument that was not expected, either here or in control
  expectArgs <- c("formula", "family", "adjust", "data", "na.action", "subset", "weights", "id", "strata", "control", names(control))
  match.idx <- match(names(Call)[-1], expectArgs)
  if(anyNA(match.idx)) warning("Unused arguments: ", paste(names(Call)[c(FALSE, is.na(match.idx))], collapse=", "), "\n")

  #### Set up "main effects" dataset ####
  indx.main <- match(c("formula", "data", "subset", "weights", "strata"), names(Call), 0L)
  if(indx.main[1] == 0) stop("A formula argument is required")
  if(length(formula) == 2) stop("'formula' should have a response variable!")

  main.call <- Call[c(1, indx.main)]
  main.call[[1]] <- quote(stats::model.frame)
  main.call$na.action <- quote(stats::na.pass) # for now, keep all rows, except for what is subset out
  if(!missing(data))
  {
    # instead of call("keep.labels", ...), which breaks when arsenal isn't loaded (Can't find "keep.labels")
    main.call$data <- as.call(list(keep.labels, main.call$data))
  }

  #### Set up "adjustment" dataset ####
  if(is.null(adjust))
  {
    adjust <- list(unadjusted = NULL)
    adjustdf <- NULL
    adjTerms <- NULL
    adjLabels <- NULL
  } else
  {
    adjust <- as_list_formula(adjust)
    if(any(lengths(adjust) != 2)) stop("'adjust' formula(s) shouldn't have a response variable!")
    if(is.null(names(adjust)))
    {
      names(adjust) <- paste0("adjusted", seq_along(adjust))
    } else if(anyDuplicated(names(adjust))) stop("Names of 'adjust' must be unique.")
    adj.call <- main.call
    adj.call$formula <- Reduce(join_formula, adjust)
    adj.call$weights <- NULL
    adj.call$strata <- NULL
    adjustdf <- eval(adj.call, parent.frame())

    Terms.a <- stats::terms(adjustdf)
    adjTerms <- make_ms_term_labels(adjustdf, Terms.a)
    adjLabels <- lapply(adjTerms, make_ms_labs)
  }

  is.numericish <- function(x) is.numeric(x) || is.Date(x)

  out.tables <- list()
  formula.list <- as_list_formula(formula)
  for(FORM in formula.list)
  {
    main.call$formula <- FORM
    maindf <- loosen.labels(eval(main.call, parent.frame()))
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

    #### Get info on y-variable ####
    yCol <- maindf[[1]]
    if(family.list$family == "gaussian" && length(unique(yCol)) <= 5) warning("Input family=gaussian, but dependent variable has 5 or fewer categories\n")

    yTerm <- colnames(maindf)[1]
    if(is.null(yLabel <- attr(yCol, "label"))) yLabel <- yTerm
    yList <- list(label = yLabel, term = yTerm)
    maindf[[1]] <- NULL
    Terms.x <- stats::delete.response(Terms)

    #### Now finish the x-variables ####
    effCols <- seq_len(ncol(attr(Terms, "factors")))
    xTerms <- make_ms_term_labels(maindf, Terms.x)

    strataList <- vector("list", length(strata.levels))
    if(hasStrata) names(strataList) <- paste0("(", strataTerm, ") == ", strata.levels)

    for(strat in strata.levels)
    {
      xList <- vector("list", length(effCols))
      names(xList) <- names(xTerms)
      idx <- if(!hasStrata) NULL else call("==", call("(", Call$strata), strat)

      for(eff in effCols)
      {
        xList[[eff]] <- vector("list", length(adjust))
        names(xList[[eff]]) <- names(adjust)

        for(adj.i in seq_along(adjust))
        {
          curr.formula <- stats::drop.terms(Terms, if(length(effCols) > 1) setdiff(effCols, eff) else NULL, keep.response = TRUE)
          adj.formula <- join_formula(curr.formula, adjust[[adj.i]])

          temp.call <- Call[c(1, match(c("data", "subset", "na.action", "weights", "id"), names(Call), 0L))]
          temp.call$formula <- adj.formula
          if(hasStrata)
          {
            temp.call$subset <- if(!is.null(temp.call$subset)) call("&", call("(", temp.call$subset), idx) else idx
          }

          currCols <- maindf[strata.col == strat, attr(Terms.x, "factors")[, eff] > 0, drop=FALSE]
          results <- modelsum_guts(family.list, temp.call, envir = parent.frame(), conf.level = control$conf.level,
                                   scope = stats::delete.response(curr.formula), anyna = anyNA(currCols))

          nmiss <- length(results$fit$na.action)
          xList[[eff]][[adj.i]] <- list(
            coeff=results$coeffTidy,
            glance = c(
              results$modelGlance,
              N = sum(strata.col == strat) - nmiss,
              Nmiss = nmiss,
              Nmiss2 = nmiss,
              endpoint=yTerm,
              endlabel=yLabel,
              x=xTerms[[eff]]$variable,
              contrasts=list(results$fit$contrasts)
            )
          )
        }
      }
      strataList[[if(!hasStrata) 1 else paste0("(", strataTerm, ") == ", strat)]] <- xList
    }

    out.tables[[yTerm]] <- list(y = yList, strata = list(term = strataTerm, values = strata.levels, label = strataLabel, hasStrata = hasStrata),
                                x = lapply(xTerms, make_ms_labs),
                                adjust = adjLabels,
                                tables = strataList, family = family.list$family, hasWeights = hasWeights)
  }

  structure(list(Call = Call, control = control, tables = out.tables), class = c("modelsum", "arsenal_table"))
}
