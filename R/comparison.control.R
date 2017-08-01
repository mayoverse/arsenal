
#' Control settings for `compare` function
#'
#' Control tolerance definitions for the \code{\link{compare.data.frame}} function.
#'
#' @param tol.logical,tol.num,tol.char,tol.factor,tol.date A function or one of the shortcut character strings,
#'   denoting the tolerance function to use for a given data type. See "details", below.
#' @param tol.num.val Numeric; maximum value of differences allowed in numerics (fed to the function given in \code{tol.num}).
#' @param int.as.num Logical; should integers be coerced to numeric before comparison? Default FALSE.
#' @param factor.as.char Logical; should factors be coerced to character before comparison? Default FALSE.
#' @param tol.date.val Numeric; maximum value of differences allowed in dates (fed to the function given in \code{tol.date}).
#' @param tol.vars Either \code{"none"} (the default), denoting that variable names are to be matched as-is, or a
#'   character vector denoting equivalence classes for characters in the variable names. See "details", below.
#' @param ... Other arguments (not in use at this time).
#' @return A list containing the necessary parameters for the \code{\link{compare.data.frame}} function.
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
#' }
#'
#' \code{tol.vars}: If not set to \code{"none"} (the default), the \code{tol.vars} argument is a character vector denoting equivalence classes
#'   for the characters in the variable names. A single character in this vector means to replace that character
#'   with \code{""}. All other strings in this vector are split by character and replaced by the first character in the string.
#'
#' E.g., a character vector \code{c("._", "aA", " ")} would denote that the dot and underscore are equivalent (to be translated to a dot),
#'   that "a" and "A" are equivalent (to be translated to "a"), and that spaces should be removed.
#'
#' The special character string \code{"case"} in this vector is the same as specifying \code{paste0(letters, LETTERS)}.
#' @examples
#' cntl <- comparison.control(
#'   tol.num = "pct",     # calculate percent differences
#'   tol.vars = c("case", # ignore case
#'                "._",   # set all underscores to dots.
#'                "e")    # remove all letter e's
#' )
#' @seealso \code{\link{compare.data.frame}}, \code{\link{comparison.tolerances}}
#' @author Ethan Heinzen
#' @export
comparison.control <- function(
  tol.logical = "none",
  tol.num = c("absolute", "percent", "pct"),
  tol.num.val = sqrt(.Machine$double.eps),
  int.as.num = FALSE,
  tol.char = c("none", "trim", "case", "both"),
  tol.factor = c("none", "levels", "labels"),
  factor.as.char = FALSE,
  tol.date = "absolute",
  tol.date.val = 0,
  tol.vars = "none", ...)
{
  #### Logical ####
  if(!is.function(tol.logical)) tol.logical <- match.fun(paste0("tol.logical.", match.arg(tol.logical, several.ok = FALSE)))

  #### Numerics ####
  if(!is.numeric(tol.num.val)) stop("'tol.num.val' needs to be numeric.")
  if(!is.function(tol.num)) tol.num <- match.fun(paste0("tol.num.", match.arg(tol.num, several.ok = FALSE)))
  if(!is.logical(int.as.num) || length(int.as.num) != 1 || is.na(int.as.num)) stop("'int.as.num' should be TRUE or FALSE.")

  #### Characters and factors ####
  if(!is.function(tol.char)) tol.char <- match.fun(paste0("tol.char.", match.arg(tol.char, several.ok = FALSE)))

  if(!is.function(tol.factor)) tol.factor <- match.fun(paste0("tol.factor.", match.arg(tol.factor, several.ok = FALSE)))
  if(!is.logical(factor.as.char) || length(factor.as.char) != 1 || is.na(factor.as.char)) stop("'factor.as.char' should be TRUE or FALSE.")

  #### Dates ####
  if(!is.numeric(tol.date.val)) stop("'tol.date.val' needs to be numeric.")
  if(!is.function(tol.date)) tol.date <- match.fun(paste0("tol.date.", match.arg(tol.date, several.ok = FALSE)))

  #### Variable names ####
  if(!is.character(tol.vars)){stop("'tol.vars' must be a character string or vector.")}
  if("none" %in% tol.vars || length(tol.vars) == 0) tol.vars <- "none"
  if("case" %in% tol.vars) tol.vars <- c(paste0(letters, LETTERS), tol.vars[tol.vars != "case"])

  return(list(tol.logical = tol.logical, tol.num = tol.num, tol.num.val = tol.num.val, int.as.num = int.as.num, tol.char = tol.char,
              tol.factor = tol.factor, factor.as.char = factor.as.char, tol.date = tol.date, tol.date.val = tol.date.val, tol.vars = tol.vars))
}
