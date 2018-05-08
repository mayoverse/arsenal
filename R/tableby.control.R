
#' Control settings for \code{tableby} function
#'
#' Control test and summary settings for the \code{\link{tableby}} function.
#'
#' @param test logical, telling \code{tableby} whether to perform tests of x variables across levels of the group variable.
#' @param total logical, telling \code{tableby} whether to calculate a column of totals across group variable.
#' @param test.pname character string denoting the p-value column name in \code{\link{summary.tableby}}.
#'   Modifiable also with \code{\link{modpval.tableby}}.
#' @param cat.simplify logical, tell \code{tableby} whether to include the first level of the categorical variable if binary.
#'   If \code{TRUE}, only the summary stats of the second level, and total (if \code{TRUE}), are calculated.
#'   NOTE: this only simplifies to one line if \code{cat.stats} is only one statistic, such as countpct.
#'   Specifically, if \code{cat.stats} includes Nmiss and there are missings, then Nmiss is included in the stats.
#' @param numeric.test name of test for numeric RHS variables in \code{tableby}: anova, kwt (Kruskal-Wallis).
#'   If no LHS variable exists, then a mean is required for a univariate test.
#' @param numeric.stats summary statistics to include for numeric RHS variables within the levels of the group LHS variable.
#'   Options are N, Nmiss, Nmiss2, mean, meansd, median, q1q3, medianq1q3, iqr, range, medianrange, or other R built-in or user-written functions.
#' @param cat.test name of test for categorical variables: chisq, fe (Fisher's Exact)
#' @param cat.stats summary statistics to include for categorical RHS variables within the levels of the group LHS variable.
#'   Options are N, Nmiss, Nmiss2, count, countpct, countrowpct, or other R built-in or user-written functions.
#' @param chisq.correct logical, correction factor for chisq.test
#' @param simulate.p.value logical, simulate p-value for categorical tests (fe and chisq)
#' @param B number of simulations to perform for simulation-based p-value
#' @param ordered.test name of test for ordered variables: trend
#' @param ordered.stats summary statistics to include for categorical RHS variables within the levels of the group LHS variable.
#'   Options are N, Nmiss, count, countpct, or other R built-in or user-written functions.
#' @param surv.test name of test for survival variables: logrank
#' @param surv.stats summary statistics to include for time-to-event (survival) RHS variables within the levels of the group LHS variable.
#'   Options are Nevents, medsurv, NeventsSurv, NriskSurv, medTime, rangeTime.
#' @param date.test name of test for date variables: kwt
#' @param date.stats stats functions to perform for Date variables: Nmiss, median, range, medianrange, q1q3, medianq1q3,
#'   or other R built-in or user-written functions.
#' @param stats.labels A named list of labels for all the statistics function names, where the function name is the named element in the list
#'   and the value that goes with it is a string containing the formal name that will be printed in all printed renderings of the output,
#'   e.g., \code{list(countpct="Count (Pct)")}.
#' @param digits Number of decimal places for numeric values.
#' @param digits.count Number of decimal places for count values.
#' @param digits.pct Number of decimal places for percents.
#' @param digits.p Number of decimal places for p-values.
#' @param format.p Logical, denoting whether to format p-values. See "Details", below.
#' @param ... additional arguments.
#' @details
#' All tests can be turned off by setting \code{test} to FALSE.
#'   Otherwise, test are set to default settings in this list, or set explicitly in the formula of \code{tableby}.
#'
#' If \code{format.p} is \code{FALSE}, \code{digits.p} denotes the number of significant digits shown. The
#'   p-values will be in exponential notation if necessary. If \code{format.p} is \code{TRUE},
#'   \code{digits.p} will determine the number of digits after the decimal point to show. If the p-value
#'   is less than the resulting number of places, it will be formatted to show so.
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
tableby.control <- function(test=TRUE,total=TRUE, test.pname=NULL, cat.simplify=FALSE,
   numeric.test="anova", cat.test="chisq", ordered.test="trend", surv.test="logrank", date.test="kwt",
   numeric.stats=c("Nmiss","meansd","range"), cat.stats=c("Nmiss","countpct"),
   ordered.stats=c("Nmiss", "countpct"), surv.stats=c("Nevents","medSurv"), date.stats=c("Nmiss", "median","range"),
   stats.labels=list(Nmiss="N-Miss", Nmiss2="N-Miss", meansd="Mean (SD)", medianq1q3="Median (Q1, Q3)", q1q3="Q1, Q3",
                     range="Range", countpct="Count (Pct)", Nevents="Events", medSurv="Median Survival",
                     medTime = "Median Follow-Up", rangeTime = "Range of Follow-Up"),
   digits = 3L, digits.count = 0L, digits.pct = 1L, digits.p = 3L, format.p = TRUE,
   chisq.correct=TRUE, simulate.p.value=FALSE, B=2000, ...) {

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

  ## validate all test names
  if(!exists(numeric.test)) {
    stop("numeric test does not exist: ", numeric.test, "\n")
  }
  if(!exists(cat.test)) {
    stop("categorical test does not exist: ", cat.test, "\n")
  }
  if(!exists(ordered.test)) {
    stop("ordinal test does not exist: ", ordered.test, "\n")
  }
  if(!exists(surv.test)) {
    stop("survival test does not exist: ", surv.test, "\n")
  }
  if(!exists(date.test)) {
    stop("date test does not exist: ", date.test, "\n")
  }
 ## validate summary stat function names

  if(any(!exists(numeric.stats))) {
    stop("One or more numeric summary statistic functions do not exist.\n")
  }
  if(any(!exists(cat.stats))) {
    stop("One or more categorical summary statistic functions do not exist.\n")
  }
  if(any(!exists(ordered.stats))) {
    stop("One or more ordered summary statistic functions do not exist.\n")
  }
  if(any(!exists(surv.stats))) {
    stop("One or more survival summary statistic functions do not exist.\n")
  }
  if(any(!exists(date.stats))) {
    stop("One or more date summary statistic functions do not exist.\n")
  }

  list(test=test, total=total, test.pname=test.pname, cat.simplify=cat.simplify,
       numeric.test=numeric.test, cat.test=cat.test,
       ordered.test=ordered.test, surv.test=surv.test,
       numeric.stats=numeric.stats, cat.stats=cat.stats,
       ordered.stats=ordered.stats,  surv.stats=surv.stats,
       date.test=date.test, date.stats=date.stats,
       chisq.correct=chisq.correct, simulate.p.value=simulate.p.value, B=B,
       stats.labels=stats.labels,
       digits=digits, digits.p=digits.p, digits.count = digits.count, digits.pct = digits.pct, format.p = format.p)
}
