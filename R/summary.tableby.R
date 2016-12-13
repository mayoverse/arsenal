## Purpose: summary method for tableby object
## Author: Greg Dougherty, Jason Sinnwell and Beth Atkinson
## Updated: 9/29/2015


format.translations <- list(Nmiss = "N-miss", Nmiss2 = "N-miss", meansd = "Mean (SD)", q1q3 = "Q1, Q3", 
							range = "Range", Nevents = "Events", medsurv = "Median Survival", 
							sex = "Sex", age = "Age")
 

#' The summary method for a \code{tableby} object
#' 
#' The summary method for a tableby object, which is a pretty rendering of a tableby object into a 
#' publication-quality results table in R-studio, and can render well in text-only.
#' 
#' @param object			An object of class \code{tableby}, made by the \code{\link{tableby}} function.
#' @param title				Title that will appear on the top of the header in the pretty-table rendering 
#'		of the tableby object
#' @param labelTranslations	All labels that are to appear in the pretty rendering of the \code{tableby} 
#'		results have both summary-statistic labels that are replaced by a formal label 
#'		(e.g., \code{meansd} by \code{"Mean (SD)"}), and the variables from the formula can be replaced 
#'		by a more formal name.
#' @param digits			Digits to round for significant digits of numeric, non-integer values. 
#'		If \code{digits.test} is not set, \code{digits} is used for that setting.
#' @param nsmall			Minimum number of digits to the right of the decimal point to display 
#'		for floating point numbers.  If \code{NA} (default), it uses the value from 
#'		\code{object$control$nsmall}. Allowed non-\code{NA} values are \code{0 <= nsmall <= 20}.
#' @param nsmall.pct		Minimum number of digits to the right of the decimal point to display 
#'		for percent numbers.  If \code{NA} (default), it uses the value from \code{object$control$nsmall.pct}.
#' @param digits.test		Significant digits by which to round for numeric test statistic p-values, 
#'		if the test was performed.
#' @param text				Logical, tell R to print the raw text version of the summary to the screen. 
#'		Default is \code{FALSE}, but recommended to be \code{TRUE} for interactive R session development.
#' @param removeBlanks		Logical, remove extra blanks in the pretty rendering of the table
#' @param labelSize			Relative size difference between label column and other columns. 
#'		Default is 1.2: label column ~20\% bigger than other columns.
#' @param test				Logical, denoting whether the "p value" value should be printed. 
#'		If \code{NA} (default), it uses the value from \code{object$control$test}.
#' @param test.pname		Title for p-value (only matters if test is \code{TRUE}; default is "p value").
#' @param pfootnote			Logical, denoting whether to add a footnote describing the test used to 
#'		generate the p value. Default is \code{FALSE}.
#' @param total				Logical, denoting whether to include the "total" value. 
#'		If \code{NA} (default), it uses the value from \code{object$control$total}.
#' @param ...				Other arguments (not in use at this time).
#' @details
#' For text-only, simply paste the summary stats together per variable, along with p-value and totals, 
#' with group variable in the header.  For other formats, the paste is done into a pandoc-style markup 
#' such that it can be translated into 3 formats: latex, html, rft.  The decision of which of those it 
#' is translated to is left for run-time for whatever format into which the report is being generated.
#' 
#' For all interative development within R sessions, we recommend \code{text=TRUE}.
#' 
#' @return Results are cat'ed to stdout, and returned invisibly as a data.frame of the tableby
#' @seealso \code{\link{tableby.control}}, \code{\link{tableby}}
#' @author Gregory Dougherty, Jason Sinnwell, Beth Atkinson, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
#' @examples
#' 
#' set.seed(100)
#' ## make 3+ categories for Response
#' nsubj <- 90
#' mdat <- data.frame(Response=sample(c(1,2,3),nsubj, replace=TRUE),
#'                    Sex=sample(c("Male", "Female"), nsubj,replace=TRUE),
#'                    Age=round(rnorm(nsubj,mean=40, sd=5)),
#'                    HtIn=round(rnorm(nsubj,mean=65,sd=5)))
#' 
#' ## allow default summaries on RHS variables
#' out <- tableby(Response ~ Sex + Age + HtIn, data=mdat)
#' summary(out, text=TRUE)
#' labels(out)
#' labels(out) <- c(Age="Age (years)", HtIn="Height (inches)")
#' summary(out, labelTranslations=c(meansd="Mean-SD"), text=TRUE)
#' 
#' @export
summary.tableby <- function (object, title = NULL, labelTranslations = NULL, digits = NA, 
							 nsmall = NA, nsmall.pct = NA, digits.test = NA, text = FALSE, 
							 removeBlanks = text, labelSize = 1.2, test = NA, test.pname = NA, 
							 pfootnote = NA, total = NA, ...) {
	frameOut <- makeSummary.tableby(TRUE, object, title, labelTranslations, digits, nsmall, 
									nsmall.pct, digits.test, text, removeBlanks, labelSize, test, 
									test.pname, pfootnote, total)
	
	invisible(frameOut)
}


#' as.data.frame.tableby
#' 
#' Build a data.frame from the tableby object and parameters, and return it
#' 
#' @inheritParams summary.tableby
#' 
#' @return Information is returned as a data.frame of the tableby
#' @author Gregory Dougherty, Jason Sinnwell, Beth Atkinson, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
#' 
#' @export
as.data.frame.tableby <- function (object, title = NULL, labelTranslations = NULL, digits = NA, 
									nsmall = NA, nsmall.pct = NA, digits.test = NA, 
									test = NA, test.pname = NA, total = NA, ...) {
	frameOut <- makeSummary.tableby(FALSE, object, title, labelTranslations, digits, nsmall, 
									nsmall.pct, digits.test, FALSE, FALSE, 1.2, test, test.pname, 
									NA, total)
	
	return(frameOut)
}


#' Format the information in object as a Table using Pandoc coding or plain text<br/>
#' If doText is TRUE, cat it to stdout, else just return the data.frame
#' 
#' @param doText			Do we print text output, or only do the data.frame
#' @inheritParams summary.tableby
#' 
#' @return Results are cat'ed to stdout, and returned invisibly as a Vector of Strings
#' 
#' @author m082166
makeSummary.tableby <- function (doText, object, title, labelTranslations, digits, nsmall, 
								 nsmall.pct, digits.test, text, removeBlanks, labelSize, test, 
								 test.pname, pfootnote, total) {
	control <- object$control
	digits <- setParam(digits, control$digits)
	digits.test <- setParam(digits.test, control$digits.test)
	nsmall <- setParam(nsmall, control$nsmall)
	nsmall.pct <- setParam(nsmall.pct, control$nsmall.pct)
	keepTotalCol <- setParam3(total, control$total, TRUE)
        if(!keepTotalCol & !any(grepl("Total", names(object$y[[1]]$stats)))) {
          ## total column already not included, so tell method to keep last col, which it thinks is Total
          keepTotalCol <- TRUE
        }
	hasPValue <- setParam3(test, control$test, TRUE)
	pValueTitle <- setParam3(test.pname, control$test.pname, "p value")
	pfootnote <- setParam3(pfootnote, control$pfootnote, FALSE)
	collapse <- setParam(control$cat.simplify, FALSE)

	if (text) {
		boldMark <- ""
		indentStr <- " "
	}
	else {
		boldMark <- "**"
		indentStr <- "&nbsp;"
		removeBlanks <- FALSE
	}

	translations <- format.addTranslations(object, labelTranslations)
	elements <- object$x
	group <- object$y[[1]]
	minColSize <- maxStrLen(lookupHumanTitle(names(elements), translations)) + (nchar(boldMark) * 2)
	minColSize <- max(minColSize, maxNameLen(elements, translations))
	
	header <- makeHeader(group, minColSize, keepTotalCol, hasPValue, pValueTitle, labelSize = labelSize)
	lineSize <- as.integer(header$lineSize)
	firstColSize <- as.integer(header$firstColSize)
	colSize <- as.integer(header$colSize)
	results <- header$header
	headers <- c("variable", header$headers)
	lastLine <- results[length(results)]	# Clip off last line, will need to add it back at end
	results <- results[- length(results)]
	if (pfootnote) {
		methods <- list()
	}
	else {
		methods <- NULL	# No footnotes are done when methods = NULL
	}
	
	frameLists <- list(term = NULL, variable = NULL)
	for (element in elements) {
          
		elmResults <- formatElement(element, lineSize, firstColSize, colSize, keepTotalCol, hasPValue, 
									translations, digits, digits.test, nsmall, nsmall.pct, 
									boldMark, indentStr, collapse, methods)
		strings <- elmResults$strings
		frameLists <- addListElement(frameLists, headers, strings, element$name, firstColSize, 
									 colSize, boldMark, indentStr)
		if (doText) {
			results <- c(results, strings, "")
			methods <- elmResults$methods
		}
	}
	
	frameOut <- makeDataFrame(headers, frameLists)
	
	if (doText) {
		results <- c(results, lastLine, "")	# Table must have blank line after last dashed line
		if (removeBlanks) {
			results <- c(results[nchar(results) > 0], "")	# Keep one blank line at end
		}
		
		if (!is.null(title) && !is.na(title)) {
			if (text) {
				results <- c(makeCenteredStr(title, lineSize), results)
			}
			else {	# Titles can be multi line, so have a blank line to clearly end title
				results <- c(paste0("Table: ", boldMark, title, boldMark), "", results)
			}
		}
		
		results <- addMethods (results, methods)
		cat(paste(results, collapse = "\n"))
	}
	return(frameOut)
}

