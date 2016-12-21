## Created: 12/13/2016
## Author: Ethan Heinzen

#' An arsenal of useful R functions for biomedical statistics
#'
#' An arsenal of useful R functions for biomedical statistics, specializing in models, reporting, and data summaries.
#'
#' @section Functions:
#' 
#' Below are listed some of the most widely used functions available in \code{arsenal}:
#'
#' \code{\link{tableby}}: Summary statistics Of a set of independent variables by a categorical variable.
#' 
#' \code{\link{modelsum}}: Fit models over each of a set of independent variables with a response variable.
#' 
#' \code{\link{freqlist}}: Approximate the output from SAS's \code{PROC FREQ} procedure when using the \code{/list} option of the \code{TABLE} statement.
#' 
#' \code{\link{write2word}}, \code{\link{write2html}}, \code{\link{write2pdf}}: Functions to generate a word, html, or pdf document containing a single table.
#' 
#' \code{\link{formulize}}: A shortcut to generate one-, two-, or many-sided formulas.
#' 
#' \code{\link{mdy.Date}} and \code{\link{Date.mdy}}: Convert numeric dates for month, day, and year to Date object, and vice versa.
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

#### commands to build the package using devtools
# devtools::document()
# devtools::check_man()
# devtools::test()
# devtools::check()
# withr::with_libpaths(c("/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/testinstalls/", .libPaths()),
#                      devtools::install("../arsenal-eph/", build_vignettes = TRUE, dependencies = FALSE))
# devtools::build("../arsenal-eph/")
## < restart R >
## library(arsenal)

#### to upload to CRAN
## Update DESCRIPTION, README.md, NEWS.md, and cran-comments.md
# devtools::revdep_check()
# devtools::release()
