###########################################################################################
### Creation Date: 6/2015
### Last Modified: Monday, 18 July 2016 03:00 PM CDT
###########################################################################################

#' Not in
#'
#' The not-in operator for R.
#'
#' @inheritParams base::`%in%`
#' @return The negation of \code{\link[base:match]{\%in\%}}.
#' @examples
#' 1 %nin% 2:10
#' c("a", "b") %nin% c("a", "c", "d")
#' @seealso \code{\link[base:match]{\%in\%}}
#' @author Raymond Moore
#' @aliases nin
#' @export
`%nin%` <- function(x, table) match(x, table, nomatch = 0L) == 0L
