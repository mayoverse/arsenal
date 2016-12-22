###########################################################################################
### Creation Date: 6/2015
### Last Modified: Monday, 18 July 2016 03:00 PM CDT
########################################################################################### 

#' Not in
#' 
#' The not-in operator for R.
#' 
#' @inheritParams base::`%in%`
#' @return The negation of \code{\link{\%nin\%}}.
#' @examples
#' 1 %nin% 2:10
#' c("a", "b") %nin% c("a", "c", "d")
#' @seealso \code{\link{\%in\%}}
#' @author Raymond Moore
#' @aliases nin
#' @export

## The not-in operator for R
## From Raymond Moore, 6/2015, who found it on google
## '%nin%' <- Negate('%in%')

## sorry guys--Ethan changed this on 7/26/16

`%nin%` <- function(x, table) match(x, table, nomatch = 0L) == 0L
