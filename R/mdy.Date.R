## Author: Terry Therneau
## Contributed on 8/30/2013
## Updated 7/23/2014 by Jason Sinnwell

#' Convert numeric dates to Date object, and vice versa
#' 
#' Convert numeric dates for month, day, and year to Date object, and vice versa.
#' 
#' @param month integer, month (1-12).
#' @param day integer, day of the month (1-31, depending on the month).
#' @param year integer, either 2- or 4-digit year. If two-digit number, will add 1900 onto it, depending on range.
#' @param yearcut cutoff for method to know if to convert to 4-digit year.
#' @param date A date value.
#' @return \code{mdy.Date} returns a Date object, and Date.mdy returns a list with integer values for month, day, and year.
#' @details More work may need to be done with yearcut and 2-digit years. Best to give a full 4-digit year.
#' @seealso \code{\link{Date}}, \code{\link{DateTimeClasses}}
#' @examples
#' mdy.Date(9, 2, 2013)
#' 
#' tmp <- mdy.Date(9, 2, 2013)
#' Date.mdy(tmp)
#' @name mdy.Date
NULL
#> NULL

#' @rdname mdy.Date
#' @export
# mdy.Date(c(0,5),c(1, 1),c(2014, 2013))  # should return NA, "2013-05-01"
mdy.Date <- function(month, day, year, yearcut=120) {
    ## keep operations vectorized
    ## NA for day or month out of range
    day <- as.numeric(day)
    day <- ifelse(day < 1 | day > 31, NA, day) # stop ("invalid day")

    month <- as.numeric(month)
    month <- ifelse(month < 1 | month > 12 | month != floor(month), NA, month)
    
    year <- ifelse(year < yearcut, year + 1900, year)
    temp <- cbind(year, month, day)  # force them all to the same length
    ## allow NAs
    dtext <- rep(NA, nrow(temp))
    dtext[rowSums(is.na(temp)) < 1] <- paste(temp[rowSums(is.na(temp)) < 1, 1, drop=FALSE],
                            sprintf("%2d", temp[rowSums(is.na(temp)) < 1, 2, drop=FALSE]),
                            sprintf("%2d", temp[rowSums(is.na(temp)) < 1, 3, drop=FALSE]), sep='/')

   as.Date(dtext)
}

#' @rdname mdy.Date
#' @export
Date.mdy <- function(date) {
    temp <- unclass(as.POSIXlt(date))
    list(month=temp$mon+1, day=temp$mday, year=1900+temp$year)
}

