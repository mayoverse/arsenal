#' freqlist
#'
#' Approximate the output from SAS's \code{PROC FREQ} procedure when using the \code{/list} option of the \code{TABLE} statement.
#'
#' @param tab an object of class \code{"table"} or class \code{"xtabs"}
#' @param sparse a logical value indicating whether to keep rows with counts of zero. The default is \code{FALSE}.
#' @param na.options a character string indicating how to handling missing values: 'include'
#'   (include values with NAs in counts and percentages),
#'   'showexclude' (show NAs but exclude from cumulative counts and all percentages),
#'   'remove' (remove values with NAs); default is 'include'
#' @param digits a single number indicating the number of digits for percentages (passed to \code{\link{round}}; default is 2.
#' @param labelTranslations an optional character string of labels to use for variable levels when summarizing.
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
#' @author Tina Gunderson
#' @name freqlist
NULL
#> NULL

#' @rdname freqlist
#' @export
freqlist <- function(tab, sparse = FALSE, na.options = c('include', 'showexclude', 'remove'), digits = 2, labelTranslations = NULL, groupBy = NULL, ...)
{
  na.options <- match.arg(na.options)
  if (any(class(tab) %nin% c("table","xtabs"))) stop("table object is not of class 'table' or class 'xtabs'")
  if (min(dim(tab)) < 1) stop("table object has dimension of 0")
  if (!is.logical(sparse)) stop("sparse must be TRUE or FALSE")
  if (length(digits) > 1) stop("digits must be a single numeric value")
  if ((digits %% 1) != 0 || (digits < 0)) stop("digits must be a positive whole number")
  if (!is.null(groupBy) && any(groupBy %nin% names(dimnames(tab)))) stop("groupBy variable not found in table names")
  if (is.list(labelTranslations)) labelTranslations <- unlist(labelTranslations)
  if (!is.null(labelTranslations) && (!is.character(labelTranslations) || length(labelTranslations) != length(dim(tab))))
    stop("length of variable names does not match table object dimensions")

  if("varnames" %in% names(list(...))){warning("The 'varnames' argument has been deprecated. Please use 'labelTranslations' instead.")}

  cumfun <- function(x) {
    # function to create a cumulative sum retaining NAs, but omitting in sum function
    x2 <- rep(NA, length(x))
    if (length(stats::na.omit(x)) == 0) {
      warning("For at least one level, all entries have NAs")
    } else {
      x2[!is.na(x)] <- cumsum(stats::na.omit(x))
    }
    return(x2)
  }
  # create data frame from table object
  if ("xtabs" %nin% class(tab)){
    tab.df <- data.frame(expand.grid(dimnames(tab)))
    oldnames <- names(tab.df)
    tab.freq <- data.frame(tab.df, Freq = as.vector(tab))
  } else {
    tab.freq <- data.frame(tab)
    oldnames <- names(tab.freq)[1:(ncol(tab.freq)-1)]
  }
  if (length(labelTranslations) > (ncol(tab.freq)-1)) stop("Number of variable names greater than number of variables")
  internalTable <- function(data, na.options = na.options, digits = digits) {
    # orders and performs calculations for the table
    # split into a function to be able to use with by statement
    data <- data[do.call(order, data), ]
    na.index <- apply(data, 1, function(x) sum(is.na(x)))
    if (na.options == 'remove') {
      data  <- subset(data, na.index == 0)
      cumFreq <- cumsum(data$Freq)
      freqPct <- 100 * data$Freq / sum(data$Freq)
      cumPct <- cumsum(freqPct)
    } else if(na.options == 'include') {
      cumFreq = cumsum(data$Freq)
      freqPct = 100 * data$Freq / sum(data$Freq)
      cumPct = cumsum(freqPct)
    } else if(na.options == 'showexclude') {
      freq_tmp <- data$Freq
      freq_tmp[na.index != 0] <- NA
      cumFreq = cumfun(freq_tmp)
      freqPct = 100 * freq_tmp / max(stats::na.omit(cumFreq), na.rm = TRUE)
      cumPct = cumfun(freqPct)
    }
    freqOut  <- data.frame(cumFreq = cumFreq, freqPercent = round(freqPct, digits), cumPercent = round(cumPct, digits))
    minitable  <- cbind(data, freqOut, row.names = NULL)
    return(minitable)
  }
  #if a grouping factor is given, will add NA as a factor level so it is not dropped when using the by function
  if(!is.null(groupBy)) {
    if(na.options != 'exclude') {
      for(i in match(groupBy, names(tab.freq))) {
        if(sum(is.na(tab.freq[[i]])) > 0) {tab.freq[[i]] <- addNA(tab.freq[[i]])}
      }
    }
    byObject <- by(tab.freq, tab.freq[, groupBy, drop = FALSE], FUN = internalTable, na.options = na.options, digits = digits)
    tableout <- do.call(rbind, byObject)
    factorIndex <- match(groupBy, names(tableout))
    tableout <- cbind(tableout[, groupBy, drop = FALSE], tableout[, -factorIndex, drop = FALSE])
    names(tableout)[1:length(groupBy)] <- groupBy
    row.names(tableout) <- NULL
    tableout <- tableout[do.call(order, tableout), ]
  } else {
    tableout <- internalTable(tab.freq, na.options = na.options, digits = digits)
  }
  if (!sparse) {
    tableout <- tableout[tableout$Freq != 0, ]
    tableout <- droplevels(tableout)
  }
  variable_labels <- labelTranslations
  if (!is.null(labelTranslations)) {
    # applies new variable names, reordering to match current data frame output
    variable_labels <- labelTranslations[match(names(tableout)[1:length(labelTranslations)], oldnames)]
  }
  outlist <- list(freqlist=tableout, byVar=groupBy, labels=variable_labels)
  class(outlist) <- "freqlist"
  return(outlist)
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
