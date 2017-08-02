## Created: 12/13/2016
## Author: Ethan Heinzen

#' An Arsenal of 'R' Functions for Large-Scale Statistical Summaries
#'
#' An Arsenal of 'R' functions for large-scale statistical summaries,
#'   which are streamlined to work within the latest reporting tools in 'R' and 'RStudio' and
#'   which use formulas and versatile summary statistics for summary tables and models.
#'
#' The package download, NEWS, and README are available on CRAN: \url{https://cran.r-project.org/package=arsenal}
#'
#' @section Functions:
#'
#' Below are listed some of the most widely used functions available in \code{arsenal}:
#'
#' \code{\link{tableby}}: Summary statistics Of a set of independent variables by a categorical variable.
#'
#' \code{\link{modelsum}}: Fit models over each of a set of independent variables with a response variable.
#'
#' \code{\link{freqlist}}: Approximate the output from SAS's \code{PROC FREQ} procedure when using
#'  the \code{/list} option of the \code{TABLE} statement.
#'
#' \code{\link{compare.data.frame}}: Compare two data.frames and report any differences between them,
#'  much like SAS's \code{PROC COMPARE} procedure.
#'
#' \code{\link{write2word}}, \code{\link{write2html}}, \code{\link{write2pdf}}: Functions to output
#'   tables to a single Word, HTML, or PDF document.
#'
#' \code{\link{write2}}: Functions to output tables to a single document.
#'   (Also the S3 backbone behind the \code{write2*} functions.)
#'
#' \code{\link{keep.labels}}: Keep the \code{'label'} attribute on an R object when subsetting.
#'
#' \code{\link{formulize}}: A shortcut to generate one-, two-, or many-sided formulas.
#'
#' \code{\link{mdy.Date}} and \code{\link{Date.mdy}}: Convert numeric dates for month, day, and year to Date object, and vice versa.
#'
#' \code{\link{is.Date}}: Test if an object is a date.
#'
#' \code{\link{\%nin\%}}: Test for "not in".
#'
#' @section Data:
#'
#' \code{\link{mockstudy}}: Mock study data for examples.
#'
#' @examples
#' library(arsenal)
#'
#' @docType package
#' @name arsenal
#'
NULL

#' @importFrom testthat compare
#' @seealso \code{\link{compare.data.frame}}
#' @export
testthat::compare

#### commands to build the package using devtools
# devtools::check_man()
# devtools::test()
# devtools::check()
# withr::with_libpaths(c("../testinstalls/", .libPaths()),
#                      devtools::install("../rpkg-arsenal/", build_vignettes = TRUE, dependencies = FALSE))
# devtools::build("../rpkg-arsenal/")
## < restart R >
## library(arsenal, lib.loc = "../testinstalls/")

#### to upload to CRAN
## Update DESCRIPTION, README.md, NEWS.md, and cran-comments.md
# devtools::revdep_check()
# devtools::release()
