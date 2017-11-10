#' freqlist
#'
#' Approximate the output from SAS's \code{PROC FREQ} procedure when using the \code{/list} option of the \code{TABLE} statement.
#'
#' @param object An R object, usually of class \code{"table"} or class \code{"xtabs"}
#' @param sparse a logical value indicating whether to keep rows with counts of zero. The default is \code{FALSE}.
#' @param na.options a character string indicating how to handling missing values: 'include'
#'   (include values with NAs in counts and percentages),
#'   'showexclude' (show NAs but exclude from cumulative counts and all percentages),
#'   'remove' (remove values with NAs); default is 'include'
#' @param digits a single number indicating the number of digits for percentages (passed to \code{\link{round}}; default is 2.
#' @param labelTranslations an optional character string (or list) of labels to use for variable levels when summarizing.
#'   Names will be matched appropriately.
#' @param groupBy an optional character string specifying a variable(s) to use for grouping when calculating cumulative
#'   counts and percentages. \code{\link{summary.freqlist}} will also separate by grouping variable for printing.
#' @param ... additional arguments passed to the \code{\link[knitr]{kable}} function
#' @param x an object of class \code{"freqlist"}
#' @return An object of class \code{"freqlist"} (invisibly for \code{print.freqlist})
#' @seealso \code{\link[base]{table}}, \code{\link[stats]{xtabs}}, \code{\link[knitr]{kable}}
#'
#' @examples
#' # load mockstudy data
#' data(mockstudy)
#' tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany")
#' noby <- freqlist(tab.ex, na.options = "include")
#' summary(noby)
#' withby <- freqlist(tab.ex, groupBy = c("arm","sex"), na.options = "showexclude")
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
freqlist.table <- function(object, sparse = FALSE, na.options = c('include', 'showexclude', 'remove'), digits = 2, labelTranslations = NULL, groupBy = NULL, ...)
{
  na.options <- match.arg(na.options)
  if (!is.table(object)) stop("'object' must be a table!")
  if (min(dim(object)) < 1) stop("table object has dimension of 0")
  if (!is.logical(sparse)) stop("sparse must be TRUE or FALSE")
  if (length(digits) > 1) stop("digits must be a single numeric value")
  if ((digits %% 1) != 0 || (digits < 0)) stop("digits must be a positive whole number")
  if (!is.null(groupBy) && any(groupBy %nin% names(dimnames(object)))) stop("groupBy variable not found in table names")
  if (is.list(labelTranslations)) labelTranslations <- unlist(labelTranslations)
  if (!is.null(labelTranslations) && (!is.character(labelTranslations) || length(labelTranslations) != length(dim(object))))
    stop("length(labelTranslations) does not match table object dimensions")

  cumfun <- function(x) {
    # function to create a cumulative sum retaining NAs, but omitting in sum function
    x2 <- rep(NA, length(x))
    x.om <- stats::na.omit(x)
    if (length(x.om) == 0) {
      warning("For at least one level, all entries have NAs")
    } else {
      x2[!is.na(x)] <- cumsum(x.om)
    }
    return(x2)
  }
  # create data frame from table object
  tab.freq <- as.data.frame(object)
  oldnames <- utils::head(names(tab.freq), -1)

  internalTable <- function(data, na.options = na.options, digits = digits) {
    # orders and performs calculations for the table
    # split into a function to be able to use with by statement
    data <- data[do.call(order, data), ]
    na.index <- rowSums(is.na(data))
    if (na.options == 'remove') {
      data  <- data[na.index == 0, ]
      cumFreq <- cumsum(data$Freq)
      freqPct <- 100 * data$Freq / sum(data$Freq)
      cumPct <- cumsum(freqPct)
    } else if(na.options == 'include') {
      cumFreq <- cumsum(data$Freq)
      freqPct <- 100 * data$Freq / sum(data$Freq)
      cumPct <- cumsum(freqPct)
    } else if(na.options == 'showexclude') {
      freq_tmp <- data$Freq
      freq_tmp[na.index != 0] <- NA
      cumFreq <- cumfun(freq_tmp)
      freqPct <- 100 * freq_tmp / max(stats::na.omit(cumFreq), na.rm = TRUE)
      cumPct <- cumfun(freqPct)
    }
    data$cumFreq <- cumFreq
    data$freqPercent <- round(freqPct, digits)
    data$cumPercent <- round(cumPct, digits)
    row.names(data) <- NULL
    return(data)
  }
  #if a grouping factor is given, will add NA as a factor level so it is not dropped when using the by function
  if(!is.null(groupBy)) {
    if(na.options != 'remove') {
      for(i in groupBy) {
        if(sum(is.na(tab.freq[[i]])) > 0) {tab.freq[[i]] <- addNA(tab.freq[[i]])}
      }
    }
    byObject <- by(tab.freq, tab.freq[, groupBy, drop = FALSE], FUN = internalTable, na.options = na.options, digits = digits)
    tableout <- do.call(rbind, byObject)
    tableout <- tableout[, c(groupBy, colnames(tableout)[colnames(tableout) %nin% groupBy]), drop = FALSE]
    row.names(tableout) <- NULL
    tableout <- tableout[do.call(order, tableout), ]
  } else {
    tableout <- internalTable(tab.freq, na.options = na.options, digits = digits)
  }
  if (!sparse) {
    tableout <- tableout[tableout$Freq != 0, ]
    tableout <- droplevels(tableout)
  }

  if (!is.null(labelTranslations)) {
    # applies new variable names, reordering to match current data frame output
    labelTranslations <- labelTranslations[match(utils::head(names(tableout), -4), oldnames)]
  }
  outlist <- list(freqlist=tableout, byVar=groupBy, labels=NULL)
  class(outlist) <- "freqlist"
  labels(outlist) <- labelTranslations
  return(outlist)
}

#' @rdname freqlist
#' @export
freqlist.formula <- function(formula, data, subset, na.action, addNA, exclude, drop.unused.levels, ...)
{
  Call <- match.call()
  indx <- match(c("formula", "data", "subset", "na.action", "addNA", "exclude", "drop.unused.levels"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required.")

  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::xtabs)

  tab <- eval(temp.call, parent.frame())
  freqlist(tab, ...)
}


#' @rdname freqlist
#' @export
print.freqlist <- function(x, ...)
{
  cat("Freqlist Object\n\n")
  cat(ncol(x$freqlist) - 4, " variables:\n", sep = "")
  print(colnames(x$freqlist)[1:(ncol(x$freqlist) - 4)])
  invisible(x)
}
