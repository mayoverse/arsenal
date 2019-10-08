
#' Control settings for \code{comparedf} function
#'
#' Control tolerance definitions for the \code{\link{comparedf}} function.
#'
#' @param tol.logical,tol.num,tol.char,tol.factor,tol.date,tol.other A function or one of the shortcut character strings or a list thereof,
#'   denoting the tolerance function to use for a given data type. See "details", below.
#' @param tol.num.val Numeric; maximum value of differences allowed in numerics (fed to the function given in \code{tol.num}).
#' @param int.as.num Logical; should integers be coerced to numeric before comparison? Default FALSE.
#' @param factor.as.char Logical; should factors be coerced to character before comparison? Default FALSE.
#' @param tol.date.val Numeric; maximum value of differences allowed in dates (fed to the function given in \code{tol.date}).
#' @param tol.vars Either \code{"none"} (the default), denoting that variable names are to be matched as-is,
#'   a named vector manually specifying variable names to compare (where the names correspond to columns of
#'   \code{x} and the values correspond to columns of \code{y}), or a
#'   character vector denoting equivalence classes for characters in the variable names. See "details", below.
#' @param max.print.vars Integer denoting maximum number of variables to report in the "variables not shared" and "variables not compared"
#'   output. \code{NA} will print all differences.
#' @param max.print.obs Integer denoting maximum number of not-shared observations to report. \code{NA} will print all differences.
#' @param max.print.diffs.per.var,max.print.diffs Integers denoting the maximum number of differences to report for each variable or overall.
#'  \code{NA} will print all differences for each variable or overall.
#' @param max.print.attrs Integers denoting the maximum number of non-identical attributes to report.\code{NA} will print all differences.
#' @param max.print.diff Deprecated.
#' @param ... Other arguments (not in use at this time).
#' @return A list containing the necessary parameters for the \code{\link{comparedf}} function.
#' @details
#' The following character strings are accepted:
#' \itemize{
#'   \item{\code{tol.logical = "none"}: compare logicals exactly as they are.}
#'   \item{\code{tol.num = "absolute"}: compare absolute differences in numerics.}
#'   \item{\code{tol.num = "percent"}, \code{tol.num = "pct"} compare percent differences in numerics.}
#'   \item{\code{tol.char = "none"}: compare character strings exactly as they are.}
#'   \item{\code{tol.char = "trim"}: left-justify and trim all trailing white space.}
#'   \item{\code{tol.char = "case"}: allow differences in upper/lower case.}
#'   \item{\code{tol.char = "both"}: combine \code{"trim"} and \code{"case"}.}
#'   \item{\code{tol.factor = "none"}: match both character labels and numeric levels.}
#'   \item{\code{tol.factor = "levels"}: match only the numeric levels.}
#'   \item{\code{tol.factor = "labels"}: match only the labels.}
#'   \item{\code{tol.date = "absolute"}: compare absolute differences in dates.}
#'   \item{\code{tol.other = "none"}: expect objects of other classes to be exactly identical.}
#' }
#'
#' A list with names mapped to \code{x} can be used to specify tolerances by variable. One unnamed element is supported
#' as the default.
#'
#' \code{tol.vars}: If not set to \code{"none"} (the default) or a named vector,
#'   the \code{tol.vars} argument is a character vector denoting equivalence classes
#'   for the characters in the variable names. A single character in this vector means to replace that character
#'   with \code{""}. All other strings in this vector are split by character and replaced by the first character in the string.
#'
#' E.g., a character vector \code{c("._", "aA", " ")} would denote that the dot and underscore are equivalent (to be translated to a dot),
#'   that "a" and "A" are equivalent (to be translated to "a"), and that spaces should be removed.
#'
#' The special character string \code{"case"} in this vector is the same as specifying \code{paste0(letters, LETTERS)}.
#' @examples
#' cntl <- comparedf.control(
#'   tol.num = "pct",     # calculate percent differences
#'   tol.vars = c("case", # ignore case
#'                "._",   # set all underscores to dots.
#'                "e")    # remove all letter e's
#' )
#'
#' comparedf.control(tol.char = list(
#'   "none",      # the default
#'   x1 = "case", # be case-insensitive for the variable "x1"
#'   x2 = function(x, y) tol.NA(x, y, x != y | y == "NA") # a custom-defined tolerance
#' ))
#' @seealso \code{\link{comparedf}}, \code{\link{comparedf.tolerances}}, \code{\link{summary.comparedf}}
#' @author Ethan Heinzen
#' @export
comparedf.control <- function(
  tol.logical = "none",
  tol.num = c("absolute", "percent", "pct"),
  tol.num.val = sqrt(.Machine$double.eps),
  int.as.num = FALSE,
  tol.char = c("none", "trim", "case", "both"),
  tol.factor = c("none", "levels", "labels"),
  factor.as.char = FALSE,
  tol.date = "absolute",
  tol.date.val = 0,
  tol.other = "none",
  tol.vars = "none",
  max.print.vars = NA, max.print.obs = NA, max.print.diffs.per.var = 10,
  max.print.diffs = 50, max.print.attrs = NA, ..., max.print.diff = 10)
{
  make_tol <- function(x, prefix, defaults)
  {
    if(is.function(x)) return(list(x))
    if(is.character(x)) return(list(match.fun(paste0(prefix, match.arg(x, defaults, several.ok = FALSE)))))
    if(!is.list(x) || (length(x) > 1 && (is.null(names(x)) || sum(names(x) == "") > 1)))
      stop("Provided tolerances should be a function, single character string, or a named list of functions/character strings")

    out <- lapply(x, function(y) {
      if(!is.function(y)) match.fun(paste0(prefix, match.arg(y, defaults, several.ok = FALSE))) else y
    })
    names(out) <- names(x)
    out
  }

  #### Logical ####
  tol.logical <- make_tol(tol.logical, "tol.logical.", "none")

  #### Numerics ####
  if(!is.numeric(tol.num.val)) stop("'tol.num.val' needs to be numeric.")
  tol.num <- make_tol(tol.num, "tol.num.", c("absolute", "percent", "pct"))
  if(!is.logical(int.as.num) || length(int.as.num) != 1 || is.na(int.as.num)) stop("'int.as.num' should be TRUE or FALSE.")

  #### Characters and factors ####
  tol.char <- make_tol(tol.char, "tol.char.", c("none", "trim", "case", "both"))

  tol.factor <- make_tol(tol.factor, "tol.factor.", c("none", "levels", "labels"))
  if(!is.logical(factor.as.char) || length(factor.as.char) != 1 || is.na(factor.as.char)) stop("'factor.as.char' should be TRUE or FALSE.")

  #### Dates ####
  if(!is.numeric(tol.date.val)) stop("'tol.date.val' needs to be numeric.")
  tol.date <- make_tol(tol.date, "tol.date.", "absolute")

  #### Other ####
  tol.other <- make_tol(tol.other, "tol.other.", "none")

  #### Variable names ####
  if(!is.character(tol.vars)) stop("'tol.vars' must be a character string or vector.")
  if(is.null(names(tol.vars)))
  {
    if("none" %in% tol.vars || length(tol.vars) == 0) tol.vars <- "none"
    if("case" %in% tol.vars) tol.vars <- c(paste0(letters, LETTERS), tol.vars[tol.vars != "case"])
  }

  if(!missing(max.print.diff))
  {
    .Deprecated(msg = "Using 'max.print.diff = ' is deprecated. Use 'max.print.diffs.per.var = ' instead.")
    max.print.diffs.per.var <- max.print.diff
  }
  chk <- function(x)
  {
    tmp <- deparse(substitute(x))
    if(length(x) != 1 || (!is.na(x) && (!is.numeric(x) || x <= 0))) stop("'", tmp, "' needs to be NA or a numeric > 0")
  }

  chk(max.print.vars)
  chk(max.print.obs)
  chk(max.print.diffs.per.var)
  chk(max.print.diffs)
  chk(max.print.attrs)

  list(tol.logical = tol.logical, tol.num = tol.num, tol.num.val = tol.num.val, int.as.num = int.as.num, tol.char = tol.char,
       tol.factor = tol.factor, factor.as.char = factor.as.char, tol.date = tol.date, tol.date.val = tol.date.val,
       tol.other = tol.other, tol.vars = tol.vars, max.print.vars = max.print.vars, max.print.obs = max.print.obs,
       max.print.diffs.per.var = max.print.diffs.per.var, max.print.diffs = max.print.diffs, max.print.attrs = max.print.attrs)
}
