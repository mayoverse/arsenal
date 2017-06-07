## Purpose: summary method for modelsum object
## Author: Greg Dougherty, Jason Sinnwell and Beth Atkinson
## Updated: 9/29/2015


## should not need these, especially for variable names
modelsum.translations <- list() ## adj.r.squared = "adj.rsq", sex = "Sex", sexM = "Sex", age = "Age")


#' Summarize a \code{modelsum} object.
#'
#' Format the information in \code{object} as a table using Pandoc coding or plain text, and cat it to stdout.
#'
#' @param object The data defining the table to display
#' @param title	Title for the table, defaults to \code{NULL} (no title)
#' @param labelTranslations A named list (or vector) where the name is the label in the
#'        output to be replaced in the pretty rendering of modelsum by the character
#'        string value for the named element of the list, e.g., 
#'        \code{list(age = "Age(years)", medsurv = "Median Survival")}. This applies to the statistic
#'        labels and the variable labels in the output.
#' @param digits Maximum number of digits to display for floating point numbers.
#' 		If \code{NA} (default), it uses the value from \code{object$control$digits}
#' 		(whose default is 3, which would result in, e.g., 12.3, 1.23, 0.123, and 0.012).
#' @param nsmall Minimum number of digits to the right of the decimal point to display for
#' 		floating point numbers.  If \code{NA} (default), it uses the value from \code{object$control$nsmall}.
#' 		Allowed non-\code{NA} values are \code{0 <= nsmall <= 20}.
#' @param nsmall.ratio Minimum number of digits to the right of the decimal point to display
#' 		for the ratio statistics (OR, HR, RR). If \code{NA} (default) it uses the value from
#' 		\code{object$control$nsmall.ratio} (whose default is 2).
#' 		Allowed values are \code{0 <= nsmall.ratio <= 20}.
#' @param digits.test Number of digits to display for a p-value. Default is 5 (e.g. 0.12345).
#' @param show.intercept Logical, denoting if the intercept should be shown for each line
#' @param show.adjust Logical, denoting if the adjust variables should be shown for each line.
#' @param text Logical, denoting whether to print out the text version.
#' @param removeBlanks Logical, denoting if any blank lines should be removed from the output.
#' 		Default is value of \code{"text"}, and will be set to \code{FALSE} if text is \code{FALSE}.
#' @param labelSize Relative size difference between label column and other columns.
#' 		Default is 1.2: label column ~20\% bigger than other columns
#' @param pfootnote Logical denoting if a footnote should be added describing the test used
#' 		to generate the p value.  Default is \code{FALSE}.
#' @param ... Other arguments (not implemented a this time).
#' @seealso \code{\link{modelsum}}, \code{\link{print.modelsum}}, \code{\link{as.data.frame.modelsum}}
#' @return Results are cat'ed to stdout, and returned invisibly as a character vector.
#' @export
#'
#' @author Greg Dougherty
summary.modelsum <- function(object, title = NULL, labelTranslations = NULL, digits = NA,
							  nsmall = NA, nsmall.ratio = NA, digits.test = NA, show.intercept = NA,
							  show.adjust = NA, text = FALSE, removeBlanks = text, labelSize = 1.2,
							  pfootnote = TRUE, ...){
	results <- makeSummary.modelsum(object, title, labelTranslations, digits, nsmall, nsmall.ratio,
									 digits.test, show.intercept, show.adjust, text, removeBlanks,
									 labelSize, pfootnote)
	results <- results$results
	cat(paste(c("", results, ""), collapse = "\n"))
	invisible(results)
}


#' as.data.frame.modelsum
#'
#' Build a data.frame from the modelsum object and parameters, and return it
#'
#' @param x An object of class \code{\link{modelsum}}.
#' @inheritParams summary.modelsum
#' @param pFootnote	Logical denoting if a footnote should be added describing the test used
#' 		to generate the p value.  Default is \code{TRUE}.
#' @return A data.frame holding the modelsum
#' @export
#'
#' @author Greg Dougherty
as.data.frame.modelsum <- function(x, ..., title = NULL, labelTranslations = NULL, digits = NA,
									nsmall = NA, nsmall.ratio = NA, digits.test = NA, show.intercept = NA,
									show.adjust = NA, pFootnote = TRUE)
{
  if(length(list(...)) > 0) warning("The '...' in this function has changed. Are you passing positional arguments?")
	results <- makeSummary.modelsum(x, title, labelTranslations, digits, nsmall, nsmall.ratio,
									 digits.test, show.intercept, show.adjust, FALSE, FALSE, 1.2, pFootnote)

	return(to.data.frame.modelsum(x, results))
}


## ' Format the information in object as a Table using Pandoc coding or plain text, & cat it to stdout
## '
## ' @inheritParams summary.modelsum
## ' @param pFootnote	Logical denoting if a footnote should be added describing the test used
## ' 		to generate the p value.  Default is \code{TRUE}.
## '
## ' @return List holding the lines of the output plus information on column size
## ' results: The lines of the output,
## ' firstColSize: The length of the first column,
## ' colSize: The length of each other column,
## ' showIntercept: TRUE if included the Intercept with each variable
## ' showAdjust: TRUE if included the adjust variable results for each variable
## ' showOnce: Vector of fields to be pulled from element$glance, one time only
## ' translations: List to use for conversion of labels
## '
## ' @author m082166
makeSummary.modelsum <- function(object, title, labelTranslations, digits, nsmall, nsmall.ratio,
								  digits.test, show.intercept, show.adjust, text, removeBlanks,
								  labelSize, pFootnote ) {
	digits <- setParam(digits, object$control$digits)
	pValueDigits <- setParam(digits.test, object$control$digits.test)
	nsmall <- setParam(nsmall, object$control$nsmall)
	nsmall.ratio <- setParam(nsmall.ratio, object$control$nsmall.ratio)
	showIntercept <- setParam3(show.intercept, object$control$show.intercept, FALSE)
	showAdjust <- setParam3(show.adjust, object$control$show.adjust, FALSE)

	if(text) {
		boldMark <- ""
	}
	else {
		boldMark <- "**"
		removeBlanks <- FALSE
	}

	fieldName = "coeff"
	oneTimeFieldName = "glance"
	translations <- addModelsumTranslations(object, labelTranslations)
	elements <- object$fits
	if(length(elements) == 0)
		return("")	# Nothing to show, no data from which to produce anything

	theCols <- getColsToShow(object$control, object$family, elements[[1]], fieldName, oneTimeFieldName, translations)
	showCols <- theCols$showCols
	showOnce <- theCols$showOnce

	## if Nmiss in showOnce and no missing, don't show Nmiss
	totMiss <- sum(sapply(object$fits, function(x) x$glance$Nmiss))
	if((totMiss == 0) & any(grepl("Nmiss$", showOnce))) {
		showOnce <- showOnce[-grep("Nmiss$", showOnce)]
	}
	minColSize <- maxStrLen(lookupHumanTitle(names(elements), translations)) + (nchar(boldMark) * 2)
	minColSize <- max(minColSize, maxMatrixNameLen(elements, translations, fieldName))
	minColSize <- max(minColSize, maxElementNameLen(elements, translations))

	header <- makeModelSumHeader(showCols, showOnce, minColSize, labelSize, translations)
	lineSize <- as.integer(header$lineSize)
	firstColSize <- as.integer(header$firstColSize)
	colSize <- as.integer(header$colSize)
	results <- header$header
	lastLine <- results[length(results)]	# Clip off last line, will need to add it back at end
	results <- results[- length(results)]

	for(element in elements) {
		elmResults <- formatModelSum(element, lineSize, firstColSize, colSize, translations, digits,
									 pValueDigits, nsmall, nsmall.ratio, boldMark, showIntercept,
									 showAdjust, showCols, showOnce, fieldName, oneTimeFieldName, pFootnote)
		results <- c(results, elmResults, "")
	}

	results <- c(results, lastLine)
	if(removeBlanks) {
		results <- results[nchar(results) > 0]
	}

	if(!is.null(title) && !is.na(title)) {
		if(text) {
			results <- c(makeCenteredStr(title, lineSize), results)
		}
		else {	# Titles can be multi line, so have a blank line to clearly end title
			results <- c(paste0("Table: ", boldMark, title, boldMark), "", results)
		}
	}

	theResults <- list(results = results, firstColSize = firstColSize, colSize = colSize, showOnce = showOnce,
						showIntercept = showIntercept, showAdjust = showAdjust, translations = translations)
	return(theResults)
}


## ' to.data.frame.modelsum
## '
## ' Take a Pandoc Table and turn it into a data frame
## '
## ' @param object		The data defining the table to display
## ' @param results		List with seven elements:
## ' 		results: The Pandoc multi-row table
## ' 		firstColSize: The length of the first column,
## ' 		colSize: The length of each other column
## ' 		showIntercept: TRUE if included the Intercept with each variable
## ' 		showAdjust: TRUE if included the adjust variable results for each variable
## ' 		showOnce: Vector of fields to be pulled from element$glance, one time only
## ' 		translations: List to use for conversion of labels
## '
## ' @return A data.frame holding the modelsum
## '
## ' @author m082166
to.data.frame.modelsum <- function(object, results)
{
	firstColSize <- as.integer(results$firstColSize)
	colSize <- as.integer(results$colSize)
	showIntercept <- results$showIntercept
	showAdjust <- results$showAdjust
	showOnce <- results$showOnce
	translations <- results$translations
	results <- results$results

	start <- 2
	end <- 3
	while(substring(results[end], 1, 1) != '-')
		end <- end + 1

	header <- strsplit(results[start:(end - 1)], " +")
	header <- compress(header)[-1] # Drop the empty first column
	if("endpoint" %in% header)
	{
		killCol <- -match("endpoint", header)
		header <- header[killCol]
		showOnce <- showOnce[match(showOnce, "endpoint", nomatch = 0) < 1]
	} else killCol <- 0
	header <- c("model", "endpoint", header)
#	header <- myStrJoin(results[start:(end - 1)], firstColSize, colSize)

	results <- results[-(1:end)] # Delete all the header
	rowCounts <- getRowInfo(object, showIntercept, showAdjust, "endpoint", translations)

	return(to.the.data.frame.modelsum(results, header, showOnce, rowCounts, firstColSize, colSize, killCol))
}


## ' to.the.data.frame.modelsum
## '
## ' Take a Pandoc Table and turn it into a data frame
## '
## ' @param results		The Pandoc multi-row table
## ' @param header		Vector with the column headers
## ' @param showOnce		Vector of fields that were pulled from element$glance, so will need to be duplicated
## ' @param rowCounts		List with one element per model of object, holding # rows & y variable
## ' @param firstColSize	The length of the first column,
## ' @param colSize		The length of each other column
## ' @param killCol		Column of results holding data to be ignored
## '
## ' @return A data.frame holding the modelsum
## '
## ' @author m082166
to.the.data.frame.modelsum <- function(results, header, showOnce, rowCounts, firstColSize, colSize, killCol)
{
	baseList <- list(term = NULL)
	holdValues <- list()
	rowNames <- c()
	curRow <- 0
	curCount <- 1
	curMax <- 0
	start <- 1
	len <- length(results) - 2 # Last two lines are not relevant -- fixed by EPH 2/2/17

	while(start <= len)
	{
		curCount <- curCount + 1
		if(curCount > curMax)
		{
			curCount <- 1
			curRow <- curRow + 1
			curMax <- rowCounts[[curRow]][1]
			curY <- rowCounts[[curRow]][2]
		}
		end <- start
		while(end <= len)
		{
			if(nchar(results[end]) == 0)
				break
			end <- end + 1
		}
		line <- myStrJoin(results[start:(end - 1)], firstColSize, colSize)
		rowNames <- c(rowNames, line[1])
		line <- line[-1]
		if(killCol < 0)
			line <- line[killCol]
		line <- c(curRow, curY, line)

		for(i in seq_len(length(line)))
		{
			title <- header[i]
			value <- line[i]
			if(title %in% showOnce)
			{
				if(value == ".")
					value <- holdValues[[title]]
				else
					holdValues[[title]] <- value
			}
			if(is.null(baseList[[title]]))
				baseList[[title]] <- c(value)
			else
				baseList[[title]] <- c(baseList[[title]], value)
		}

		start <- end + 1
	}

	return(makeDF(baseList, rowNames, c("endpoint")))
}


## ' makeDF
## '
## ' Create a data.frame, updating the "term" column to have the rowNames as its elements, and
## ' setting its columns to be numeric or not as appropriate
## '
## ' @param baseList		List holding the contents of the data.frame to be
## ' @param rowNames		Names of the rows, will be added as column "term"
## ' @param nonNumeric	Names of non-numeric columns of the resulting data.frame
## '
## ' @return A data.frame holding the modelsum, with every column not in nonNumeric made numeric
## '
## ' @author m082166
makeDF <- function(baseList, rowNames, nonNumeric)
{
	baseList[["term"]] <- rowNames
#	df <- as.data.frame(baseList, stringsAsFactors = FALSE, row.names = rowNames)
	df <- as.data.frame(baseList, stringsAsFactors = FALSE)

	oldwarn <- options()$warn
	options(warn = -1)

	nonNumeric <- c("term", nonNumeric)
	theNames <- names(baseList)
	theNames <- theNames[!(theNames %in% nonNumeric)]

	for(colName in theNames)
		class(df[, colName]) <- "numeric"

	options(warn = oldwarn)

	return(df)
}


## ' getRowInfo
## '
## ' For each model, get the number of rows that are part of that model, and the y variable associated
## ' with it
## '
## ' @param object		The data defining the table to display
## ' @param showIntercept	TRUE if included the Intercept with each variable
## ' @param showAdjust	TRUE if included the adjust variable results for each variable
## ' @param yCol			Name of the column in glance that holds the y column value
## ' @param translations Translations
## '
## ' @return List with one element per model of object, holding # rows & y variable
## '
## ' @author m082166
getRowInfo <- function(object, showIntercept, showAdjust, yCol, translations)
{
	if(showIntercept) {
		neverRows <- c()
	}
	else {
		neverRows <- c("(Intercept)")
	}

	results <- list()
	numModels <- length(object$fits)

	for(model in seq_len(numModels))
	{
		theFit <- object$fits[[model]];
		if(showAdjust)  {
			hideRows <- neverRows
		}
		else {
			hideRows <- c(neverRows, lookupHumanTitle(theFit$adjterms, translations))
		}

		coeff <- theFit$coeff
		coeff <- coeff[!(coeff$term %in% hideRows), ]
		results[[model]] <- c(nrow(coeff), theFit$glance[[yCol]])
	}

	return(results)
}


## ' myStrJoin
## '
## ' Take potentially split strings, or vector of strings, and join back together, each column becoming
## ' a single string
## '
## ' @param theStr		String, or vector of strings, to join
## ' @param firstColSize	The length of the first column
## ' @param colSize		The length of each other column
## '
## ' @return Contents of each column de-Pandoced and formed into a single un-padded string
## '
## ' @author m082166
myStrJoin <- function(theStr, firstColSize, colSize)
{
	len <- length(theStr)
	if(len == 0)
		return(c())

	strLen <- nchar(theStr[1])
	row <- singleSplit(theStr[1], firstColSize, colSize, strLen)
	if(len == 1)
		return(stringr::str_trim(row))

	results <- stringr::str_trim(row, "left")
	numCols <- length(row)
	for(i in 2:len)
	{
		row <- singleSplit(theStr[i], firstColSize, colSize, strLen)

		for(j in seq_len(numCols))
		{
			size <- nchar(results[j])
			toAdd <- row[j]
			if(size == 0)
				results[j] <- stringr::str_trim(toAdd, "left")
			else
			{
				cur <- stringr::str_trim(results[j], "right")
				doPad <- (nchar(cur) < size) && !endsWithPad(cur)
				size <- nchar(toAdd)
				toAdd <- stringr::str_trim(toAdd, "left")
				addSize <- nchar(toAdd)
				doPad <- (doPad || (addSize < size)) && (addSize > 0) && !beginsWithPad(toAdd)
				if(doPad)
					results[j] <- paste(cur, toAdd)
				else
					results[j] <- paste0(cur, toAdd)
			}
		}
	}

	return(stringr::str_trim(results, "right"))
}


## ' singleSplit
## '
## ' Split a string based on column lengths
## '
## ' @param theStr		String to split
## ' @param firstColSize	The length of the first column
## ' @param colSize		The length of each other column
## ' @param strLen		Total string length
## '
## ' @return Contents of each column de-Pandoced but not trimmed
## '
## ' @author m082166
singleSplit <- function(theStr, firstColSize, colSize, strLen)
{
	first <- substring(theStr, 1, firstColSize)
	for(find in c("**", "&nbsp;"))
		first <- gsub(find, " ", first, fixed = TRUE)

	results <- c(first)

	start <- firstColSize + 2
	while(start < strLen)
	{
		end <- start + colSize - 1
		nextStr <- substring(theStr, start, end)
		results <- c(results, nextStr)
		start <- end + 2
	}

	return(results)
}


## ' addUniqueName
## '
## ' Add a row name, making it unique if necessary
## '
## ' @param rowNames	Vector to add to, and test against
## ' @param newRow	New row name to add
## '
## ' @return rowNames with newRow concatenated as a unique text string
## '
## ' @author m082166
addUniqueName <- function(rowNames, newRow)
{
	if(newRow %in% rowNames)
	{
		count <- 1
		test <- paste(newRow, count)
		while(test %in% rowNames)
		{
			count <- count + 1
			test <- paste(newRow, count)
		}

		newRow <- test
	}

	return(c(rowNames, newRow))
}


## Take list of 1+ elements of vectors of equal length, produce a vector with things combined
## Example: [[1]] [1] "man " "woman" "child"
## 			[[2]] [1] "kind" "" "hood"
## --> [1] "man kind" "woman" "childhood"
##
## ' compress
## '
## ' Take list of 1+ elements of vectors of equal length, produce a vector with things combined
## '
## ' @param theList	The list of data to compress
## '
## ' @return The Strings concatenated together
## '
## ' @author m082166
compress <- function(theList)
{
	if(length(theList) == 0)
		return("")	# Nothing to show, no data from which to produce anything

	result <- c()

	for(i in seq_len(length(theList[[1]])))
		result <- c(result, "")

	for(element in theList)
	{
		for(i in seq_len(length(element)))
			result[i] <- paste0(result[i], element[i])
	}

	return(result)
}


## ' addModelsumTranslations
## '
## ' Add all the desired translations from machine produced labels to a human readable ones to the list
## '
## ' @param object			The data defining the table to display
## ' @param labelTranslations	List where name is the label in the output, and value is the label you
## ' want displayed e.g. list(q1q3: "Q1, Q3", medsurv = "Median Survival")
## ' @return Current translation list
## '
## ' @author m082166
addModelsumTranslations <- function(object, labelTranslations) {
	translations <- format.addTranslations(object, labelTranslations, modelsum.translations, "fits", "xterms")

	elements <- object$fits

	# Now get every adjterms and their matching adjlabels
	for(element in elements) {
		translations <- addTranslations(translations, element$adjterms, element$adjlabels)
	}

	return(translations)
}


## ' makeModelSumHeader
## '
## ' Make the Pandoc format header for the table
## '
## ' @param showCols List of items that will be output as columns from each row, in order
## ' @param showOnce List of items that will be output as columns only on 1st row, in order
## ' @param minColSize Minimum size of the first column (which will hold label info for a row)
## ' @param translations The List to use for conversion of labels
## ' @param leftJustify	If TRUE, will left justify each column, defaults to FALSE
## ' @param rightJustify If TRUE, will right justify each column, defaults to FALSE
## ' When both leftJustify and rightJustify are FALSE, columns are centered
## ' @param labelSize	Relative size difference between label column and other columns.
## ' 		Default is 1.2: label column ~20\% bigger than other columns
## ' @return List holding the lines of the header defined by group plus
## ' lineSize: The length of a full line,
## ' firstColSize: The length of the first column,
## ' colSize: The length of each other column,
## ' header: The lines of the header
## ' The last element is the last line of the output, to go after the body of the output
## '
## ' @author m082166
makeModelSumHeader <- function(showCols, showOnce, minColSize, labelSize = 1.2,
				translations, leftJustify = FALSE, rightJustify = FALSE) {
	headers <- c(showCols, showOnce)
	size <- max(nchar(headers)) + 2	# Need one extra "-" on either side to center text
	if(size < 10)
		size <- 10	# Minimum width for a column
	bigSize = round(size * labelSize)	# Want the first column ~ 20% larger than other columns
	if(bigSize < minColSize) {
		bigSize <- minColSize
		size <- round(bigSize / labelSize)
	}
	fullSize = bigSize + ((size + 1) * length(headers))
	outsideLine <- makeDashStr(fullSize)

	header <- outsideLine
	#First Line
	head <- makeDashStr(bigSize, theChar = ' ')

	for(cellH in headers) {	# Want it to insert space as separator
		head <- paste(head, makeCellHeader(cellH, size, leftJustify, rightJustify))
	}

	header <- c(header, head)
	#Second line
	head <- makeDashStr(bigSize)

	for(cellH in headers) {
		head <- paste(head, makeDashStr(size))	# Want it to insert space as separator
	}

	header <- c(header, head)
	header <- c(header, outsideLine)

	# Build a named List with the data to return
	header <- list(lineSize = fullSize, firstColSize = bigSize, colSize = size, header = header)

	return(header)
}


## ' formatModelSum
## '
## ' Return a List with two elements:
## ' The vector holding the lines of a row in the table, defined by element, in Pandoc format
## ' Updated list of methods used by this modelsum object
## '
## ' @param element		List to get information from, whose fieldName item must be the statistics
## ' @param lineSize		Length each non-blank line should be padded to
## ' @param firstColSize	Length the first (label) column should be padded to
## ' @param colSize		Length all other columns should be padded to
## @param hasPValue TRUE if has column for p-values, FALSE if shouldn't
## ' @param translations	The List to use for conversion of labels
## ' @param digits		Maximum number of digits to display for floating point numbers
## ' @param pValueDigits	Number of digits to display for a p-value. Example: 5 ==> in 0.12345
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @param nsmall.ratio		Minimum number of digits to the right of the decimal point to display
## ' for the ratio statistics (OR, HR, RR)
## ' @param boldMark		String to use to mark text as bold
## ' @param showIntercept	TRUE if should show the Intercept for each line, FALSE if shouldn't
## ' @param showAdjust	If TRUE show all rows, if false hide the "adjust" rows
## ' @param showCols		List of items that will be output as columns from each row, in order
## ' @param showOnce		List of items that will be output as columns only on 1st row, in order
## ' @param fieldName		The name of the List element that holds the matrix with the headers
## ' @param oneTimeFieldName ...?
## ' @param pFootnote		If TRUE add a footnote describing the test used to generate the p value
## ' @return Vector of strings holding Pandoc code to create a row in a table representing element
## '
## ' @author m082166
formatModelSum <- function(element, lineSize, firstColSize, colSize, translations, digits,
                           pValueDigits, nsmall, nsmall.ratio, boldMark, showIntercept, showAdjust,
                           showCols, showOnce, fieldName, oneTimeFieldName, pFootnote)
{
  if(showAdjust) {
    hideRows <- c()
  }
  else {
    hideRows <- lookupHumanTitle(element$adjterms, translations)
  }

  coefficients <- element[[fieldName]]
  rows <- makeModelSumTitleCells(element, fieldName, firstColSize, translations, boldMark,
                                  showIntercept, hideRows)

  numRows <- dim(coefficients)[1]
  rowTitles <- lookupHumanTitle(element[[fieldName]]$term, translations)
  colTitles <- getMatrixNames(element, fieldName, doRow = FALSE, translations = translations)
  useCols <- getColsToUse(showCols, colTitles)
  volueCols <- match(showCols, "p.value", nomatch = 0)
  showExtras <- TRUE
  curRow <- 0

  for(startRow in seq_len(numRows))
    {
      if(showRow(showIntercept, hideRows, rowTitles[startRow]))
        {
          extra <- c()
          if(showExtras)
            {
              for(item in showOnce) {
                extra <- c(extra, element[[oneTimeFieldName]][[item]])
              }
              showExtras <- FALSE
            }
          else {
            for(item in showOnce) {
              extra <- c(extra, ".")
            }
          }

          rows <- addModel(rows, useCols, volueCols, coefficients[startRow, ], extra, curRow,
                           colSize, digits, pValueDigits, nsmall, nsmall.ratio)
          curRow <- curRow + 1
        }
    }

  return(rows)
}


## ' getColsToShow
## '
## ' Takes the Vector of the currently filled in rows, as well as rows that have been started but
## ' not yet completed, and fills in the modelSum info for one more row
## '
## ' @param control			Control object, holding the columns we're displaying
## ' @param family			Stats family of object we're displaying
## ' @param element			1st element we're displaying, to get column names / locations
## ' @param fieldName			The name of the List element for the "show every row" data
## ' @param oneTimeFieldName	The name of the List element for the "show once" data
## ' @param translations		The List to use for conversion of labels
## ' @return List of Vectors of names of rows to output
## ' showCols: fields to be pulled from every row of element$coeff
## ' showOnce: fields to be pulled from element$glance, one time only
## '
## ' @author m082166
getColsToShow <- function(control, family, element, fieldName, oneTimeFieldName, translations)
{
	## Match control stats columns by family to the stats in the object.
	## It is split by showCols and showOnce
	if(family %in% c("quasibinomial","binomial"))
		statFields <- control$binomial.stats
	else if(family %in% c("quasipoisson","poisson"))
		statFields <- control$poisson.stats
	else if(family == "survival")
		statFields <- control$survival.stats
	else
		statFields <- control$gaussian.stats

	statFields <- statFields[!is.na(statFields)]

	names <- colnames(element[[fieldName]])
	showCols <- names[match(statFields, names)]
	names <- names(element[[oneTimeFieldName]])
	showOnce <- names[match(statFields, names)]
	showCols <- lookupHumanTitle(showCols, translations)
	showOnce <- lookupHumanTitle(showOnce, translations)
	showCols <- showCols[!is.na(showCols)]
	showOnce <- showOnce[!is.na(showOnce)]

	return(list(showCols = showCols, showOnce = showOnce))
}


## ' showRow
## '
## ' Determines whether or not a row should be displayed
## '
## ' @param showIntercept	TRUE if should show the Intercept for each line, FALSE if shouldn't
## ' @param hideRows		Vector of names of rows that should not be shown
## ' @param rowTitle		Name of the current row
## ' @return TRUE if should show this row, FALSE if shouldn't
## '
## ' @author m082166
showRow <- function(showIntercept, hideRows, rowTitle) {
	if(rowTitle == "(Intercept)")
		return(showIntercept)

	if(is.null(hideRows))
		return(TRUE)

	return(!(rowTitle %in% hideRows))
}


## ' addModel
## '
## ' Takes the Vector of the currently filled in rows, as well as rows that have been started but
## ' not yet completed, and fills in the modelSum info for one more row
## '
## ' @param rows			Vector of strings to edit, and possibly add to
## ' @param useCols		Vector of column numbers from modelSum to use, in order to use them
## ' @param volueCols		Vector of 0s and at most 1 "1", specifying if any column is the value col
## ' @param modelSum		The row of the coefficients of modelsum to process, holding the info to add
## ' @param extra			Vector of strings to pad and add to end of the row
## ' @param curRow		Current row to operate on, 0 based
## ' @param colSize		Width to pad each cell to
## ' @param digits		Number of digits to round to when displaying percent or Other data
## ' @param pValueDigits	Number of digits to display for a p-value. Example: 5 ==> in 0.12345
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @param nsmall.ratio		Minimum number of digits to the right of the decimal point to display
## ' for the ratio statistics (OR, HR, RR)
## ' @return The updated rows
## '
## ' @author m082166
addModel <- function(rows, useCols, volueCols, modelSum, extra, curRow, colSize, digits,
					  pValueDigits, nsmall, nsmall.ratio)
{
	for(i in seq_len(length(useCols)))
	{
		col <- useCols[i]
		if(volueCols[i] > 0) {
			cell <- makeLimitedNumber(modelSum[col], pValueDigits)
		}
		else {
			cell <- myFormat(modelSum[col], digits, nsmall)
		}

		cell <- makePaddedStr(cell, colSize)
		rows <- addToRow(rows, curRow, cell)
	}

	for(item in extra) {
		if(is.numeric(item)) {
			cell <- myFormat(item, digits, nsmall)
		}
		else {
			cell <- item
		}

		cell <- makePaddedStr(cell, colSize)
		rows <- addToRow(rows, curRow, cell)
	}

	return(rows)
}


## ' getColsToUse
## '
## ' Generate a Vector holding the numbers of the columns to be used, in the order to be used
## '
## ' @param showCols		List of items that will be output as columns from each row, in order
## ' @param colTitles		List of column titles for modelSum, so can determine which to display
## ' @return Columns to be usedm in order
## '
## ' @author m082166
getColsToUse <- function(showCols, colTitles)
{
	numCells <- length(colTitles)
	unusedCols <- seq_len(numCells)
	colsToUse <- c()

	for(theCol in showCols)
	{
		for(test in seq_len(length(unusedCols)))
		{
			i <- unusedCols[test]
			colTitle <- colTitles[i]
			if(colTitle == theCol)
			{
				colsToUse <- c(colsToUse, i)
				unusedCols <- unusedCols[- test]	# Delete the used item
				break
			}
		}
	}

	return(colsToUse)
}


## ' makeModelSumHeaders
## '
## ' Make the unpadded header for each column other than the label column
## '
## ' @param object		The data that will be turned into a table
## ' @param showCols		List of items that will be output as columns from each row, in order
## ' @param showOnce		List of items that will be output as columns only on 1st row, in order
## ' @param translations	The List to use for conversion of labels
## ' @return A Vector of the column headers, given the data in object and extras,
## ' skipping the first (blank, label) header
## '
## ' @author m082166
makeModelSumHeaders <- function(object, showCols, showOnce, translations) {
  ## second arg was fieldName, but not passed (tried by JPS on 12/15/16
  theNames <- getMatrixNames(object, showCols, translations = translations)

  if(is.na(extras) || (length(extras) == 0))	# length(NA) == 1
    return(theNames)

  extras <- lookupHumanTitle(extras, translations)
  return(c(theNames, extras))
}


## ' makeModelSumTitleCells
## '
## ' Return an array of the lines needed to make the label cell, given the data in element,
## ' taking into account the maximum allowed width specified by colSize,
## ' which must be >= 4 + length of the name of element
## '
## ' @param element		List to get information from, whose first item must be the statistics
## ' @param colSize		Width to pad the output to
## ' @param translations	The List to use for conversion of labels
## ' @param boldMark		String to use to mark something as bold
## ' @param showIntercept	TRUE if should show the Intercept for each line, FALSE if shouldn't
## ' @param hideRows		Vector of names of rows that should not be shown
## ' @param fieldName	...?
## ' @return	Vector holding the strings necessary to represent the rows of element,
## ' each row separated by a blank string
## '
## ' @author m082166
makeModelSumTitleCells <- function(element, fieldName, colSize, translations, boldMark,
                                   showIntercept, hideRows) {
  theCells <- NULL
  rowTitles <- lookupHumanTitle(element[[fieldName]]$term, translations)

  for(title in rowTitles) {
    showThis <- showRow(showIntercept, hideRows, title)
    if(showThis && (title != "(Intercept)")) {
      title <- paste0(boldMark, title, boldMark)
    }

    if(showThis) {
      if(length(theCells) == 0) {
        theCells <- c(makePaddedStr(title, colSize))
      }
      else {
                                        # Add blank line to mark new row, then title line(s)
        theCells <- c(theCells, "", makePaddedStr(title, colSize))
      }
    }
  }

  return(theCells)
}


## ' getMatrixNames
## '
## ' Get the row or column names for the matrix in fieldName
## '
## ' @param element A List of Lists
## ' @param fieldName The name of the List element of interest
## ' @param doRow If TRUE get row names, if FALSE get column names, defaults to FALSE
## ' @param translations The List to use for conversion of labels, so can use the proper name length
## ' @return Vector of Strings, the row or column headers of the matrix
## '
## ' @author m082166
getMatrixNames <- function(element, fieldName, doRow = FALSE, translations = NULL) {
	if(doRow) {
		which <- 1
	}
	else {
		which <- 2
	}

	names <- dimnames(element[[fieldName]])[[which]]
	if(!is.null(translations)) {
		names <- lookupHumanTitle(names, translations)
	}

	return(names)
}


## ' maxMatrixNameLen
## '
## ' Return the length of the longest string among the names of elements
## '
## ' @param elements		A List of Lists
## ' @param translations	The List to use for conversion of labels, so can use the proper name length
## ' @param fieldName		The name of the List element of interest
## ' @param doRow			If TRUE get row names, if FALSE get column names, defaults to FALSE
## ' @return The nchar length of the longest name from element's sub-lists,
## ' as translated via translations
## '
## ' @author m082166
maxMatrixNameLen <- function(elements, translations, fieldName, doRow = FALSE) {
	theMax = 0

	for(element in elements) {
		theMax <- max(theMax, nchar(getMatrixNames(element, fieldName, doRow, translations)))
	}

	return(theMax)
}


## ' maxElementNameLen
## '
## ' Return the length of the longest string among the names of elements
## '
## ' @param elements		A List of Lists
## ' @param translations	The List to use for conversion of labels, so can use the proper name length
## ' @return The nchar length of the longest name from element's sub-lists,
## ' as translated via translations
## '
## ' @author m082166
maxElementNameLen <- function(elements, translations) {
	if(length(elements) == 0)
		return(0)

	return(max(0, nchar(lookupHumanTitle(names(elements[[1]]), translations))))
}


## JPS added 5/16/2016
## skeleton that runs, but needs updating
summary.modelsumList <- function(object, title = NULL, labelTranslations = NULL, digits = NA,
                                  nsmall = NA, nsmall.ratio = NA, digits.test = NA, show.intercept = NA,
                                  show.adjust = NA, text = FALSE, removeBlanks = text, labelSize = 1.2,
                                  pFootnote = TRUE, ...) {

	## summary on a list of modelsum objects
	for(k in 1:length(object)) {
		summary.modelsum(object[k])
	}

	invisible(object)
}
