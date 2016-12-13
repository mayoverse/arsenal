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
#' \code{\link{tableby}}: Summary Statistics Of a Set of Independent Variables By a Categorical Variable.
#' 
#' \code{\link{modelsum}}: Fit models over each of a set of independent variables with a response variable.
#' 
#' \code{\link{freqlist}}: Approximate the output from SAS's PROC FREQ procedure when using the /list option of the TABLE statement.
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
##
