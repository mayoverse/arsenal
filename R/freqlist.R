#' freqlist
#'
#' Approximate the output from SAS's \code{PROC FREQ} procedure when using the \code{/list} option of the \code{TABLE} statement.
#'
#' @param object An R object, usually of class \code{"table"} or class \code{"xtabs"}
#' @param na.options a character string indicating how to handling missing values: \code{"include"}
#'   (include values with NAs in counts and percentages),
#'   \code{"showexclude"} (show NAs but exclude from cumulative counts and all percentages),
#'   \code{"remove"} (remove values with NAs); default is \code{"include"}.
#' @param strata (formerly \code{groupBy}) an optional character string specifying a variable(s) to use for grouping when calculating cumulative
#'   counts and percentages. \code{\link{summary.freqlist}} will also separate by grouping variable for printing. Note that this is different
#'   from \code{modelsum} and \code{tableby}, which take bare column names (and only one, at that!)
#' @param labelTranslations an optional character string (or list) of labels to use for variable levels when summarizing.
#'   Names will be matched appropriately.
#' @param control control parameters to handle optional settings within \code{freqlist}. See \code{\link{freq.control}}
#' @param ... additional arguments. In the formula method, these are passed to the table method. These are also passed to
#'   \code{\link{freq.control}}
#' @param formula,data,subset,na.action,addNA,exclude,drop.unused.levels Arguments passed to \code{\link[stats]{xtabs}}. Note
#'   that \code{addNA=} only works in R >= 3.4.0.
#' @return An object of class \code{c("freqlist", "arsenal_table")}
#' @seealso \code{\link{arsenal_table}}, \code{\link{summary.freqlist}}, \code{\link{freq.control}}, \code{\link{freqlist.internal}},
#'   \code{\link[base]{table}}, \code{\link[stats]{xtabs}}
#'
#' @examples
#' # load mockstudy data
#' data(mockstudy)
#' tab.ex <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA = "ifany")
#' noby <- freqlist(tab.ex, na.options = "include")
#' summary(noby)
#'
#' # show the top 6 rows' frequencies and percents
#' head(summary(sort(noby, decreasing = TRUE)[c(1:4, 6)]))
#'
#' withby <- freqlist(tab.ex, strata = c("arm","sex"), na.options = "showexclude")
#' summary(withby)
#' @author Tina Gunderson, with revisions by Ethan Heinzen
#' @name freqlist
NULL
#> NULL

#' @rdname freqlist
#' @export
freqlist <- function(object, ...)
{
  UseMethod("freqlist")
}

#' @rdname freqlist
#' @export
freqlist.table <- function(object, na.options = c("include", "showexclude", "remove"), strata = NULL, labelTranslations = NULL, control = NULL, ...)
{
  control <- c(list(...), control)
  control <- do.call("freq.control", control[!duplicated(names(control))])

  Call <- match.call()
  na.options <- match.arg(na.options)
  if(min(dim(object)) < 1) stop("table object has dimension of 0")
  if("groupBy" %in% names(list(...)))
  {
    if(is.null(strata)) strata <- list(...)$groupBy
    .Deprecated(msg = "Using 'groupBy = ' is deprecated. Use 'strata = ' instead.")
  }

  hasStrata <- !is.null(strata)
  if(hasStrata && any(strata %nin% names(dimnames(object)))) stop("strata variable not found in table names")

  # all this just to keep non-syntactic names
  to_df <- function(x)
  {
    data.frame(
      do.call("expand.grid", c(dimnames(provideDimnames(x, sep = "", base = list(LETTERS))),
                               KEEP.OUT.ATTRS = FALSE, stringsAsFactors = TRUE)),
      Freq = c(x), row.names = NULL, check.names = FALSE
    )
  }

  tab.freq <- to_df(object)

  if(hasStrata && is.null(names(dimnames(object))) && is.null(names(labelTranslations)))
  {
    if(length(labelTranslations) != ncol(tab.freq) - 1) stop("'labelTranslations' doesn't appear to be the same length as 'object'")
    names(labelTranslations) <- utils::head(names(tab.freq), -1)
  }

  #### x variables (which might include strata) ####
  xTerms <- lapply(utils::head(names(tab.freq), -1), function(nm) list(variable=nm, label=nm, term=nm))
  names(xTerms) <- vapply(xTerms, "[[", NA_character_, "variable")

  #if a grouping factor is given, will add NA as a factor level so it is not dropped when using the by function
  if(hasStrata) {
    if(na.options != 'remove') tab.freq[strata] <- lapply(tab.freq[strata], function(x) if(anyNA(x)) addNA(x) else x)

    tableout <- unclass(by(tab.freq, tab.freq[rev(strata)], FUN = internalTable, na.options = na.options))
    tableout <- lapply(tableout, function(x) {
      x <- x[c(strata, colnames(x)[colnames(x) %nin% strata])]
      row.names(x) <- NULL
      x
    })
    xTerms <- xTerms[c(strata, names(xTerms)[names(xTerms) %nin% strata])]

    strata.levels <- ""
    strataLabel <- strata

  } else {
    tableout <- list(internalTable(tab.freq, na.options = na.options))
    strata <- strata.levels <- strataLabel <- ""
  }

  out.tables = list(
    list(
      y = list(term = "", label = ""),
      strata = list(term = strata, values = strata.levels, label = strataLabel, hasStrata = hasStrata),
      x = add_freqlist_xterms(xTerms),
      tables = unname(tableout),
      hasWeights = FALSE,
      na.options = na.options
    )
  )

  out <- structure(list(Call = Call, control = control, tables = out.tables), class = c("freqlist", "arsenal_table"))
  if(!is.null(labelTranslations)) labels(out) <- labelTranslations
  out
}

#' @rdname freqlist
#' @export
freqlist.formula <- function(formula, data, subset, na.action, na.options = c("include", "showexclude", "remove"),
                             strata = NULL, labelTranslations = NULL, control = NULL, ...)
{
  control <- c(list(...), control)
  control <- do.call("freq.control", control[!duplicated(names(control))])

  Call <- match.call()
  na.options <- match.arg(na.options)

  if("groupBy" %in% names(list(...)))
  {
    if(is.null(strata)) strata <- list(...)$groupBy
    .Deprecated(msg = "Using 'groupBy = ' is deprecated. Use 'strata = ' instead.")
  }

  indx <- match(c("formula", "data", "subset", "na.action", "addNA", "exclude", "drop.unused.levels"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required.")
  formula.list <- as_list_formula(formula)
  out.tables <- list()
  for(FORM in formula.list)
  {
    temp.call <- Call[c(1, indx[1:4])]
    temp.call[[1L]] <- quote(stats::model.frame)
    temp.call$formula <- FORM

    if(!missing(data)) temp.call$data <- as.call(list(keep.labels, temp.call$data))
    modeldf <- loosen.labels(eval(temp.call, parent.frame()))
    Terms <- stats::terms(modeldf)
    hasStrata <- !is.null(strata)
    if(hasStrata && any(strata %nin% names(modeldf))) stop("strata variable not found in table names")

    #### Check for strata ####
    if(hasStrata)
    {
      strata.levels <- ""
      strata.terms <- strata
      strataLabel <- unname(vapply(strata, function(x) if(is.null(labelEff <- attr(modeldf[[x]], "label"))) x else labelEff, NA_character_))
    } else strata.terms <- strata.levels <- strataLabel <- ""

    strataList <- list(term = strata.terms, values = strata.levels, label = strataLabel, hasStrata = hasStrata)

    #### Check for weights ####

    if(hasWeights <- attributes(Terms)$response != 0)
    {
      termBy <- names(modeldf)[1]
      if(is.null(labelBy <- attr(modeldf[[1]], "label"))) labelBy <- termBy
      yList <- list(term = termBy, label = labelBy)
      modeldf[[1]] <- NULL
    } else yList <- list(term = "", label = "")

    #### x variables (which might include strata) ####
    xTerms <- Map(modeldf, names(modeldf), f = function(col, nm) {
      if(is.null(labelEff <- attr(col, "label"))) labelEff <- nm
      list(variable=nm, label=labelEff, term=nm)
    })
    names(xTerms) <- vapply(xTerms, "[[", NA_character_, "variable")
    if(hasStrata) xTerms <- xTerms[c(strata, names(xTerms)[names(xTerms) %nin% strata])]

    ####

    temp.call <- Call[c(1, indx)]
    temp.call[[1L]] <- quote(stats::xtabs)
    temp.call$formula <- FORM
    if(indx[5] == 0) temp.call$addNA <- TRUE

    tab <- freqlist(eval(temp.call, parent.frame()), strata = strata, na.options = na.options, ...)$tables[[1]]
    tab$hasWeights <- hasWeights
    tab$y <- yList
    tab$strata <- strataList
    tab$x <- add_freqlist_xterms(xTerms)

    # this should still work even if there's multiple LHS -- test that with list(, , y) ~ x
    out.tables[[yList$term]] <- tab
  }

  out <- structure(list(Call = Call, control = control, tables = out.tables), class = c("freqlist", "arsenal_table"))
  if(!is.null(labelTranslations)) labels(out) <- labelTranslations
  out
}

