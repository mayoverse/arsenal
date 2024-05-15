
#' Control settings for \code{tableby} function
#'
#' Control test and summary settings for the \code{\link{tableby}} function.
#'
#' @param test logical, telling \code{tableby} whether to perform tests of x variables across levels of the group variable.
#' @param total logical, telling \code{tableby} whether to calculate a column of totals across group variable.
#' @param total.pos One of \code{"before"} or \code{"after"}, denoting where to put the total column relative to the by-variable columns.
#' @param test.pname character string denoting the p-value column name in \code{\link{summary.tableby}}.
#'   Modifiable also with \code{\link{modpval.tableby}}.
#' @param cat.simplify,ordered.simplify logical, tell \code{tableby} whether to remove the first level of the categorical/ordinal variable if binary.
#'   If \code{TRUE}, only the summary stats of the second level are reported (unless there's only one level, in which case it's reported).
#'   If \code{"label"}, the second level's label is additionally appended to the label.
#'   NOTE: this only simplifies to one line if there is only one statistic reported, such as \code{countpct}.
#'   In particular, if \code{Nmiss} is specified and there are missings, then the output is not simplified.
#' @param cat.droplevels Should levels be dropped for categorical variables? If set to true, p-values will not be displayed
#'   unless \code{test.always = TRUE} as well.
#' @param numeric.simplify,date.simplify logical, tell \code{tableby} whether to condense numeric/date output to a single line.
#'   NOTE: this only simplifies to one line if there is only one statistic reported, such as \code{meansd}.
#'   In particular, if \code{Nmiss} is specified and there are missings, then the output is not simplified.
#' @param numeric.test name of test for numeric RHS variables in \code{tableby}: anova, kwt (Kruskal-Wallis), medtest (median test).
#'   If no LHS variable exists, then a mean is required for a univariate test.
#' @param numeric.stats,cat.stats,ordered.stats,surv.stats,date.stats,selectall.stats summary statistics to include for the respective class of RHS variables
#'  within the levels of the group LHS variable.
#' @param cat.test name of test for categorical variables: chisq, fe (Fisher's Exact)
#' @param wilcox.correct,wilcox.exact See \code{\link[stats]{wilcox.test}}
#' @param chisq.correct logical, correction factor for chisq.test
#' @param simulate.p.value logical, simulate p-value for categorical tests (fe and chisq)
#' @param B number of simulations to perform for simulation-based p-value
#' @param ordered.test name of test for ordered variables: trend
#' @param surv.test name of test for survival variables: logrank
#' @param date.test name of test for date variables: kwt
#' @param selectall.test name of test for date variables: notest
#' @param stats.labels A named list of labels for all the statistics function names, where the function name is the named element in the list
#'   and the value that goes with it is a string containing the formal name that will be printed in all printed renderings of the output,
#'   e.g., \code{list(countpct="Count (Pct)")}. Any unnamed elements will be ignored. Passing \code{NULL} will disable labels.
#' @param digits Number of decimal places for numeric values.
#' @param digits.count Number of decimal places for count values.
#' @param digits.pct Number of decimal places for percents.
#' @param digits.p Number of decimal places for p-values.
#' @param format.p Logical, denoting whether to format p-values, or character, a \code{\link[glue]{glue}} specification for how to format. See "Details", below.
#' @param digits.n Number of decimal places for N's in the header. Set it to NA to suppress the N's.
#' @param conf.level Numeric, denoting what confidence level to use for confidence intervals.
#'   (See, e.g., \code{\link{binomCI}})
#' @param times A vector of times to use for survival summaries.
#' @param test.always Should the test be performed even if one or more by-group has 0 observations? Relevant
#'   for kwt and anova.
#' @param ... additional arguments.
#' @details
#' All tests can be turned off by setting \code{test} to FALSE.
#'   Otherwise, test are set to default settings in this list, or set explicitly in the formula of \code{tableby}.
#'
#' If \code{format.p} is \code{FALSE}, \code{digits.p} denotes the number of significant digits shown. The
#'   p-values will be in exponential notation if necessary. If \code{format.p} is \code{TRUE},
#'   \code{digits.p} will determine the number of digits after the decimal point to show. If the p-value
#'   is less than the resulting number of places, it will be formatted to show so. If \code{format.p} is a character string,
#'   it will be treated as a \code{\link[glue]{glue}} specification: the p-value is exposed as "p", and "digits.p" as "digits.p".
#'
#' Options for statistics are described more thoroughly in the vignette and are listed in \link{tableby.stats}
#'
#'
#' @return A list with settings to be used within the \code{tableby} function.
#'
#' @seealso \code{\link[stats]{anova}}, \code{\link[stats]{chisq.test}}, \code{\link{tableby}}, \code{\link{summary.tableby}},
#'   \code{\link{tableby.stats}}.
#' @author Jason Sinnwell, Beth Atkinson, Ethan Heinzen, Terry Therneau, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
#' @examples
#' set.seed(100)
#' ## make 3+ categories for Response
#' mdat <- data.frame(Response=c(0,0,0,0,0,1,1,1,1,1),
#'                    Sex=sample(c("Male", "Female"), 10,replace=TRUE),
#'                    Age=round(rnorm(10,mean=40, sd=5)),
#'                    HtIn=round(rnorm(10,mean=65,sd=5)))
#'
#' ## allow default summaries in RHS variables, and pass control args to
#' ## main function, to be picked up with ... when calling tableby.control
#' outResp <- tableby(Response ~ Sex + Age + HtIn, data=mdat, total=FALSE, test=TRUE)
#' outCtl <- tableby(Response ~ Sex + Age + HtIn, data=mdat,
#'                   control=tableby.control(total=TRUE, cat.simplify=TRUE,
#'                   cat.stats=c("Nmiss","countpct"),digits=1))
#' summary(outResp, text=TRUE)
#' summary(outCtl, text=TRUE)
#' @export
tableby.control <- function(
  test=TRUE,total=TRUE, total.pos = c("after", "before"), test.pname=NULL,
  numeric.simplify=FALSE, cat.simplify=FALSE, cat.droplevels=FALSE, ordered.simplify=FALSE, date.simplify=FALSE,
  numeric.test="anova", cat.test="chisq", ordered.test="trend", surv.test="logrank", date.test="kwt", selectall.test="notest",
  test.always = FALSE,
  numeric.stats=c("Nmiss","meansd","range"), cat.stats=c("Nmiss","countpct"),
  ordered.stats=c("Nmiss", "countpct"), surv.stats=c("Nmiss", "Nevents","medSurv"), date.stats=c("Nmiss", "median","range"),
  selectall.stats=c("Nmiss", "countpct"),
  stats.labels = list(),
  digits = 3L, digits.count = 0L, digits.pct = 1L, digits.p = 3L, format.p = TRUE, digits.n = 0L, conf.level = 0.95,
  wilcox.correct = FALSE, wilcox.exact = NULL,
  chisq.correct=FALSE, simulate.p.value=FALSE, B=2000, times = 1:5, ...) {

  nm <- names(list(...))
  if("digits.test" %in% nm) .Deprecated(msg = "Using 'digits.test = ' is deprecated. Use 'digits.p = ' instead.")
  if("nsmall" %in% nm) .Deprecated(msg = "Using 'nsmall = ' is deprecated. Use 'digits = ' instead.")
  if("nsmall.pct" %in% nm) .Deprecated(msg = "Using 'nsmall.pct = ' is deprecated. Use 'digits.pct = ' instead.")

  ## validate digits
  # digits and digits.test are OK to be NULL. See ?format
  if(!is.null(digits) && digits < 0L)
  {
    warning("digits must be >= 0. Set to default.")
    digits <- 3L
  }
  if(!is.null(digits.count) && digits.count < 0L)
  {
    warning("digits.count must be >= 0. Set to default.")
    digits.count <- 0L
  }
  if(!is.null(digits.pct) && digits.pct < 0L)
  {
    warning("digits.pct must be >= 0. Set to default.")
    digits.pct <- 1L
  }
  if(!is.null(digits.p) && digits.p < 0L)
  {
    warning("digits.p must be >= 0. Set to default.")
    digits.p <- 3L
  }
  if(!is.null(digits.n) && !is.na(digits.n) && digits.p < 0L)
  {
    warning("digits.n must be >= 0 or NA or NULL. Set to default.")
    digits.n <- 0L
  }

  stats.labels <- if(is.null(stats.labels)) NULL else add_tbc_stats_labels(stats.labels)

  list(test=test, total=total, total.pos = match.arg(total.pos), test.pname=test.pname,
       numeric.simplify=numeric.simplify, cat.simplify=cat.simplify, cat.droplevels = cat.droplevels, ordered.simplify=ordered.simplify, date.simplify=date.simplify,
       numeric.test=numeric.test, cat.test=cat.test, ordered.test=ordered.test, surv.test=surv.test, date.test=date.test, selectall.test=selectall.test,
       test.always=test.always,
       numeric.stats=numeric.stats, cat.stats=cat.stats, ordered.stats=ordered.stats, surv.stats=surv.stats, date.stats=date.stats, selectall.stats=selectall.stats,
       stats.labels=stats.labels,
       digits=digits, digits.p=digits.p, digits.count = digits.count, digits.pct = digits.pct, format.p = format.p, digits.n = digits.n,
       conf.level=conf.level,
       wilcox.correct = wilcox.correct, wilcox.exact = wilcox.exact,
       chisq.correct=chisq.correct, simulate.p.value=simulate.p.value, B=B, times=times)
}

add_tbc_stats_labels <- function(x) {
  start <- list(
    Nmiss="N-Miss", Nmiss2="N-Miss", Nmisspct="N-Miss (%)", Nmisspct2="N-Miss (%)",
    meansd="Mean (SD)", meanse = "Mean (SE)", meanpmsd="Mean &pm; SD", meanpmse = "Mean &pm; SE", medianrange="Median (Range)",
    median="Median", medianq1q3="Median (Q1, Q3)", q1q3="Q1, Q3", iqr = "IQR",
    mean = "Mean", sd = "SD", var = "Var", max = "Max", min = "Min", meanCI = "Mean (CI)", sum = "Sum",
    gmean = "Geom Mean", gsd = "Geom SD", gmeansd = "Geom Mean (Geom SD)", gmeanCI = "Geom Mean (CI)",
    range="Range", Npct="N (%)", Nrowpct="N (%)", Nevents="Events", medSurv="Median Survival",
    medSurvQuant="Median (Q1, Q3) Survival", medSurvCI="Median (CI)",
    medTime = "Median Follow-Up", medianmad="Median (MAD)", Nsigntest = "N (sign test)",
    overall = "Overall", total = "Total", difference = "Difference"
  )
  nms <- setdiff(names(x), "")
  start[nms] <- x[nms]
  start
}
