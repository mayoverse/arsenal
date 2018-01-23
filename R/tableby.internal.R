## Purpose: internal functions (and methods) for tableby function
## Authors: Jason Sinnwell, Beth Atkinson, Gregory Dougherty
## Created: 4/16/2015

## Helper functions for tableby:  merge, subset, and labels (work like names)

## merge two tableby objects
## both must have same "by" variable and levels
## if some RHS variables have same names, keep both, the one in y add ".y"

#' Helper functions for tableby
#'
#' A set of helper functions for \code{\link{tableby}}.
#'
#' @param object A \code{data.frame} resulting form evaluating \code{modelsum} formula.
#' @param ... Other arguments, or a vector of indices for extracting.
#' @param x,y A \code{tableby} object.
#' @param i A vector to index \code{x} with: either names of variables, a numeric vector, or a logical vector of appropriate length.
#' @param value A list of new labels.
#' @param pdata A named data.frame where the first column is the x variable names matched by name, the second is the
#'   p-values (or some test stat), and the third column is the method name (optional)
#' @param use.pname Logical, denoting whether the column name in \code{pdata} corresponding to the p-values should be used
#'   in the output of the object.
#' @return \code{na.tableby} returns a subsetted version of \code{object} (with attributes).
#' @name tableby.internal
NULL
#> NULL

#' @rdname tableby.internal
#' @export
merge.tableby <- function(x, y, ...) {

  if(names(x$y) != names(y$y)) {
    stop("tableby objects cannot be merged unless same 'by' variable name).\n")
  }
  if(!all(names(x$y[[1]]$stats) == names(y$y[[1]]$stats))){
    stop("tableby objects cannot be merged unless same 'by' variable categories.\n")
  }
  newobj <- x
  y$y[[1]]$label <- paste0(y$y[[1]]$label, ".2")
  newobj$y[[paste0(names(y$y)[[1]],".2")]] <- y$y[[1]]
  for(xname in names(y$x)) {
    thisname <- xname
    ## if name already present, add "2" to name and add on
    if(xname %in% names(newobj$x)) {
      thisname <- paste0(xname, ".2")
      y$x[[xname]]$label <- paste0(y$x[[xname]]$label, ".2")
    }
    newobj$x[[thisname]] <- y$x[[xname]]
  }

  ## add on call and control from y
  newobj$Call2 <- y$Call
  newobj$control2 <- y$control

  return(newobj)
}

## pdata is a named data.frame where the first column is the x variable names matched by name,
## p-values (or some test stat) are numbers and the name is matched
## method name is the third column (optional)
## to the x variable in the tableby object (x)

#' @rdname tableby.internal
#' @export
modpval.tableby <- function(x, pdata, use.pname=FALSE) {
  ## set control$test to TRUE
  if(any(pdata[,1] %in% names(x$x))) {
    x$control$test <- TRUE

    ## change test results
    for(k in 1:nrow(pdata)) {
      xname <- pdata[k,1]
      idx <- which(names(x$x)==xname)
      if(length(idx)==1) {
        x$x[[idx]]$test$p.value <- pdata[k,2]
        if(ncol(pdata)>2) {
          x$x[[idx]]$test$method <- pdata[k,3]
        } else {
          x$x[[idx]]$test$method <- "modified by user"
        }
      }
    }
    if(use.pname & nchar(names(pdata)[2])>0) {
      ## put different test column name in control
      x$control$test.pname <- names(pdata)[2]
    }
  }
  return(x)
}

## Get the labels from the tableby object's elements in the order they appear in the fomula/Call
## including the y and x variables
# labels <- function(x) {
#   UseMethod("labels")
# }

## retrieve variable labels (y, x-vec) from tableby object

#' @rdname tableby.internal
#' @export
labels.tableby <- function(object, ...) {
  ##  get the formal labels from a tableby object's data variables
  allLabels <- c(sapply(object$y, function(obj) obj$label), sapply(object$x, function(obj) obj$label))
  names(allLabels) <- c(names(object$y), names(object$x))
  return(allLabels)
}

## define generic function for tests, so tests(tbObj) will work

#' @rdname tableby.internal
#' @export
tests <- function(x) {
  UseMethod("tests")
}

## retrieve the names of the tests performed per variable

#' @rdname tableby.internal
#' @export
tests.tableby <- function(x) {
  if(x$control$test) {
    testdf <- data.frame(Variable=labels(x)[-1],
                     p.value=sapply(x$x, function(z) z$test$p.value),
                     Method=sapply(x$x, function(z) z$test$method))
    if(!is.null(x$control$test.pname)) {
      names(testdf)[2] <- x$control$test.pname
    }
  } else {
    testdf <- cat("No tests run on tableby object\n")
  }
  return(testdf)
}


## assign labels to tableby object

#' @rdname tableby.internal
#' @export
'labels<-.tableby' <- function(x, value) {
  ## if the value vector is named, then assign the labels to
  ## those names that match those in x and y
  if(!is.null(names(value))) {
    vNames <- names(value)
    objNames <- c(names(x$y), names(x$x))
    v2objIndex <- match(vNames, objNames)
    if(any(is.na(v2objIndex))) {
      warning("Named value(s): ", paste(vNames[is.na(v2objIndex)],collapse=","),
              " not matched in x\n")
    }
    ## handle y label first, then remove it
    if(any(v2objIndex==1)) {
      x$y[[1]]$label <- value[which(v2objIndex==1)]
      value <- value[-which(v2objIndex==1)]
      v2objIndex <- v2objIndex[-which(v2objIndex==1)]
    }
    if(length(v2objIndex)>0) {
      ## prepare to iterate over the rest for x, if there are any
      v2objIndex <- v2objIndex - 1
      for(k in seq_len(length(v2objIndex))) {
        x$x[[ v2objIndex[k] ]]$label <- value[k]
      }
    }
  } else  {

    ## Otherwise, assign in the order of how variables appear in formula, starting with y
    ## check that length of value matches what is expected for x
    ## for each of the RHS vars of x (1:length(x)-3),
    ##assign strings in value to the 'label' element of the list for each RHS variable

    if(length(value) != length(x$y + length(x$x))) {
      stop("Length of new labels is not the same length as there are variables in the formula.\n")
    }
    x$y[[1]]$label <- value[1]
    for(k in 1:length(x$x)) {
      x$x[[k]]$label <- value[k-1]
    }
  }
  ## return tableby x with updated labels
  return(x)
}

## subset a tableby object;
## syntax of usage: newtb <- tbObj[1:2]
## x here is the tableby object
## index is in '...', and allows only 1 vector of integer indices
## in future, maybe allow subsetting by names

#' @rdname tableby.internal
#' @export
"[.tableby" <- function(x, i) {
  if(missing(i)) return(x)
  newx <- x

  if(is.character(i) && any(i %nin% names(x$x)))
  {
    tmp <- paste0(i[i %nin% names(x$x)], collapse = ", ")
    warning(paste0("Some indices not found in tableby object: ", tmp))
    i <- i[i %in% names(x$x)]
  } else if(is.numeric(i) && any(i %nin% seq_along(x$x)))
  {
    tmp <- paste0(i[i %nin% seq_along(x$x)], collapse = ", ")
    warning(paste0("Some indices not found in tableby object: ", tmp))
    i <- i[i %in% seq_along(x$x)]
  } else if(is.logical(i) && length(i) != length(x$x))
  {
    stop("Logical vector index not the right length.")
  }

  if(length(i) == 0 || anyNA(i)) stop("Indices must have nonzero length and no NAs.")

  newx$x <- x$x[i]
  return(newx)
}



## function to handle na.action for tableby formula, data.frame

#' @rdname tableby.internal
#' @export
na.tableby <- function(object, ...) {
    omit <- is.na(object[,1])
    xx <- object[!omit, , drop = FALSE]
    if(any(omit > 0L)) {
        temp <- stats::setNames(seq(omit)[omit], attr(object, "row.names")[omit])
        attr(temp, "class") <- "omit"
        attr(xx, "na.action") <- temp
    }
    xx
}

wtd.table <- function(x, weights = rep(1, length(x)), na.rm = TRUE)
{
  tmp <- tapply(weights, x, sum, na.rm = na.rm)
  tmp[is.na(tmp)] <- 0 # (tapply(default = 0) would be enough in R >= 3.4, but we'll make this backwards-compatible)
  tmp
}

wtd.mean <- function(x, weights = NULL, na.rm = TRUE) {
    if(!length(weights)) return(mean(x, na.rm = na.rm))
    if(na.rm) {
        idx <- !is.na(x + weights)
        x <- x[idx]
        weights <- weights[idx]
    }
    sum(weights * x)/sum(weights)
}
wtd.quantile <- function(x, weights=NULL, probs=c(0,0.25,0.5,0.75,1), na.rm=TRUE) {

  if(!length(weights)) return(stats::quantile(x, probs = probs, na.rm = na.rm))
  if(any(probs < 0) || any(probs > 1)) stop("Probabilities must be between 0 and 1 inclusive")

  wts <- wtd.table(x, weights, na.rm = na.rm)
  x <- as.numeric(names(wts))
  n <- sum(wts)
  order <- 1 + (n - 1) * probs
  low <- pmax(floor(order), 1)
  high <- pmin(low + 1, n)
  order <- order%%1
  allq <- stats::approx(cumsum(wts), x, xout = c(low, high), method = "constant", f = 1, rule = 2)$y
  k <- length(probs)
  stats::setNames((1 - order) * allq[1:k] + order * allq[-(1:k)], probs)
}

wtd.var <- function(x, weights = NULL, na.rm=TRUE, method = c("unbiased", "ML")) {
    method <- match.arg(method)
    if(!length(weights)) return(stats::var(x, na.rm = na.rm))

    if(na.rm) {
        idx <- !is.na(x + weights)
        x <- x[idx]
        weights <- weights[idx]
    }
    as.numeric(stats::cov.wt(matrix(x, ncol = 1), weights, method = method)$cov)
}


########## Note from Ethan: if we want these doc pages, just replace all instances of "## '" with "#'"

## ' makeDataFrame
## '
## ' Make the tableby data frame and add the output to it
## '
## ' @param headers		Vector of most of the columns and their titles
## ' @param frameLists	List of lists holding the data for the data frame
## ' @return Data Frame with all the elements set up and filled in
## '
## ' @author m082166
makeDataFrame <- function(headers, frameLists) {
	df <- as.data.frame (frameLists, stringsAsFactors = FALSE)
	colnames(df) <- c ("term", headers)
	return (df)
}


## ' addListElement
## '
## ' Make lists for the data frame the output will be added to
## '
## ' @param theFrame		List of Lists holding the Lists to be added to, 1st time through just
## '		contains "term" and "variable", both NULL)
## ' @param headers		Vector of the columns and their titles
## ' @param rows			Vector of text to be processed
## ' @param varName		Name of the variable that created all these rows
## ' @param firstColSize	Length the first (label) column was padded to
## ' @param colSize		Length all other columns were padded to
## ' @param boldMark		Text used to indicate something is bold text.  Ignored if empty
## ' @param indentStr		Text used to indent text.  Ignored if " "
## ' @return List of Lists updated with the data from the passed in rows
## '
## ' @author m082166
addListElement <- function(theFrame, headers, rows, varName, firstColSize, colSize, boldMark, indentStr) {
	numRows <- length(rows)
	lineSize <- max(nchar(rows))
	colSize <- colSize + 1	# Add 1 for the between column spacer
	numCols <- max((lineSize - firstColSize) / colSize, length(headers))
	curCols <- length(theFrame)
	cols <- c()
	which <- 1

	while (which <= numRows) {
		line <- rows[which]
		if(line == "") {	# Empty line means start of new variable
			break
		}
		whichCol <- 1
		start <- 1
		end <- firstColSize
		while (whichCol <= numCols) {
			col <- stringr::str_trim(substr(line, start, end))

			if(length(cols) > whichCol) {
				if(nchar(col) > 0) {
					cols[whichCol] <- stringr::str_trim(paste(cols[whichCol], col))
				}
			}
			else {	# Always add, even if adding an empty string
				cols <- c(cols, col)
			}

			start <- end + 2
			end <- end + colSize
			whichCol <- whichCol + 1
		}

		which <- which + 1
	}

	name <- getName(cols[1], boldMark, indentStr)
	theFrame <- addToListVector (theFrame, "term", name)
	theFrame <- addToListVector (theFrame, headers[1], varName)
	whichCol <- 2
	while (whichCol <= numCols) {
		theFrame <- addToListVector (theFrame, headers[whichCol], cols[whichCol])
		whichCol <- whichCol + 1
	}

	if(which <= numRows) {	# If stopped with more variables to go, process them now
		for (i in 1:which) {
			rows = rows[-1]	# Remove rows we've done
		}
		return(addListElement(theFrame, headers, rows, varName, firstColSize, colSize - 1, boldMark, indentStr))
	}

	return(theFrame)
}


## ' addToListVector
## '
## ' If baseList[[title]] is NULL, make it a Vector holding value.  If it's not null, make a
## '   vector holding its contents followed by value
## '
## ' @param baseList	List to update
## ' @param title		Name of baseList element to update
## ' @param value		Text to add to the Vector at baseList[[title]]
## ' @return baseList after it has been updated
## '
## ' @author m082166
addToListVector <- function(baseList, title, value) {
	if(is.null (baseList[[title]]))
		baseList[[title]] <- c (value)
	else
		baseList[[title]] <- c (baseList[[title]], value)

	return (baseList)
}


## ' getName
## '
## ' Extract the row name from the text
## '
## ' @param nameText	Text to parse
## ' @param boldMark	Text used to indicate something is bold text.  Ignored if empty
## ' @param indentStr	Text used to indent text.  Ignored if " "
## ' @return String holding the ceaned up text.  Will clean either boldMark or indentStr
## '
## ' @author m082166
getName <- function(nameText, boldMark, indentStr) {
	bSize <- nchar(boldMark)
	tSize <- nchar(nameText)

	if(bSize > 0) {
		if(tSize > (bSize * 2)) {

			if((boldMark == substr(nameText, 1, bSize)) &&
				(boldMark == substr(nameText, tSize - bSize + 1, tSize))) {
				return(substr(nameText, bSize + 1, tSize - bSize))
			}
		}
	}

	if(indentStr != " ") {	# Trim takes care of a space indentStr
		iSize <- nchar(indentStr)
		while ((tSize > iSize) && (indentStr == substr(nameText, 1, iSize))) {
			nameText <- substr(nameText, iSize + 1, tSize)
			tSize <- tSize - iSize
		}
	}

	nameText <- stringr::str_trim(nameText)
	return(nameText)
}


## ' process
## '
## ' Process text, extracting the numbers and returning them as a list of Strings
## '
## ' @param theText Text to parse
## ' @return List of strings holding numbers, possibly including a %
## '
## ' @author m082166
process <- function(theText) {
	locations <- stringr::str_locate_all(theText, "-*[0-9.%]+")[[1]]
	numResults <- nrow(locations)
	results <- c()

	for (i in seq_len(numResults)) {
		results <- c(results, substr(theText, locations[i, 1], locations[i, 2]))
	}

	if(length(results) == 0) {
		results = ""
	}

	return(c(results))
}


## ' addMethods
## '
## ' Add the methods to the table output
## '
## ' @param results	Vector of strings to add to, will add immediately to end of results
## ' @param methods	List of Methods, where the names are method names, and the values are the
## ' order the methods appear in the output
## ' @return Vector of strings holding Pandoc code to create the table and its methods, if any
## '
## ' @author m082166
addMethods <- function(results, methods) {
	theNames <- names(methods)
	if(is.null(theNames)) {
		return(results)
	}

	outOrder <- c()
	for (aName in theNames) {
		which <- methods[[aName]]
		outOrder[as.integer(which)] <- aName
	}

	which <- 1
	for (aMethod in outOrder) {
		results <- c(results, paste0(which, ". ", aMethod))
		which <- which + 1
	}

	results <- c(results, "")
	return(results)
}


## ' formatElement
## '
## ' Return a List with two elements:
## ' The vector holding the lines of a row in the table, defined by element, in Pandoc format
## ' Updated list of methods used by this tableby object
## '
## ' @param element		List to get information from, whose first item must be the statistics
## ' @param lineSize		Length each non-blank line should be padded to
## ' @param firstColSize	Length the first (label) column should be padded to
## ' @param colSize		Length all other columns should be padded to
## ' @param includeTotal	TRUE if should include last pre-pValue column, FALSE if shouldn't
## ' @param hasPValue		TRUE if has column for p-values, FALSE if shouldn't
## ' @param translations	The List to use for conversion of labels
## ' @param digits		Maximum number of digits to display for floating point numbers
## ' @param pValueDigits	Number of digits to display for a p-value. Example: 5 ==> in 0.12345
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @param pctNSmall		Minimum number of digits to the right of the decimal point to display for
## ' percent numbers.  If NULL, use 'nsmall', if that is NULL use 'digits' to determine everything
## ' @param boldMark		String to use to mark text as bold
## ' @param indentStr		String to use to indent something one space
## ' @param collapse		If true, data might have been collapsed, and needs to be tested
## ' @param methods		List of methods and when they were first seen, or NULL if not tracking that
## ' @return List of two elements:
## ' strings: Vector of strings holding Pandoc code to create a row in a table representing element
## ' methods: Updated list of methods, or NULL if methods was NULL
## '
## ' @author m082166
formatElement <- function(element, lineSize, firstColSize, colSize, includeTotal, hasPValue,
							translations, digits, pValueDigits, nsmall, pctNSmall, boldMark,
							indentStr, collapse, methods) {
	statistics <- element$stats	# GTD 10/14/15 was statistics <- element[[1]]
	collapse <- collapse && (numStats(statistics) == 1) && (element$type == "categorical")
	rows <- makeTitleCell(element, firstColSize, translations, boldMark, indentStr, collapse)
	outputTypes <- element$output
	isDate <- !is.na(element$type) && !is.null(element$type) && (element$type == "Date")

	if(collapse) {
		rows <- addStatistic(rows, statistics[[1]], 0, 1, colSize, outputTypes, isDate, digits,
							 nsmall, pctNSmall, includeTotal)
	}
	else {
		startRow <- 0
		whichStat <- 0

		for (stat in statistics) {
			startRow <- startRow + 1
			whichStat <- whichStat + 1
			rows <- addStatistic(rows, stat, startRow, whichStat, colSize, outputTypes, isDate,
								 digits, nsmall, pctNSmall, includeTotal)
		}
	}

	if(hasPValue) {
		ref <- makeReference(methods, as.character(element$test$method))
		endText <- ref$ref
		methods <- ref$methods
		rows[1] <- addNumberToEnd(rows[1], element$test$p.value, lineSize, digits = pValueDigits,
								  endText = endText)
	}

	result <- list(strings = rows, methods = methods)
	return(result)
}


## ' numStats
## '
## ' Count the number of Elements represented by statistics.  If only one element in statistics,
## ' count number of elements in that element
## '
## ' @param statistics	List holding the data of interest
## ' @return Count of elements in statistics.  If that is 1, count of names in that element
## '
## ' @author m082166
numStats <- function(statistics) {
	size <- length(statistics)
	if(size != 1)
		return(size)

	numRows <- length(row.names(statistics[[1]][[1]]))

	if(numRows > 0)
		return(numRows)

	return(size)
}


## ' makeReference
## '
## ' Return a list with two elements:
## ' The text specifying the superscript referencing the method used this time
## ' Updated list of methods used by this tableby object
## '
## ' @param methods	List of methods and when they were first seen, or NULL if not tracking that
## ' @param method	Method used this time
## ' @return List of two elements:
## ' ref: Text specifying the superscript for the passed in method
## ' methods: Updated list of methods, or NULL if methods was NULL
## '
## ' @author m082166
makeReference <- function(methods, method) {
	endText <- ""
	if(!is.null(methods)) {
		if(!is.na(method)) {
			if(length(methods) == 0) {
				ref <- NULL
			}
			else {
				ref <- methods[[method]]
			}
			if(is.null(ref)) {
				ref <- length(methods) + 1
				methods[[method]] <- ref
			}
			endText <- paste0('^', ref, '^')
		}
	}

	result <- list(ref = endText, methods = methods)
	return(result)
}


## ' addStatistic
## '
## ' Takes the Vector of the currently filled in rows, as well as rows that have been started but
## ' not yet completed, and fills in the stats info for one more row
## '
## ' @param rows			Vector of strings to edit, and possibly add to
## ' @param stat			The Stats element to process, holding the info to add
## ' @param startRow		Current row to operate on, 0 based
## ' @param whichStat		Which of the items in outputTypes to use
## ' @param colSize		Width to pad each cell to
## ' @param outputTypes	Vector from which to pull output type
## ' @param isDate		If true, show data as dates, if false treat normally
## ' @param digits		Number of digits to round to when displaying percent or Other data
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @param pctNSmall		Minimum number of digits to the right of the decimal point to display for
## ' percent numbers
## ' @param includeTotal	TRUE if should include last pre-pValue column, FALSE if shouldn't
## ' @return The updated rows
## '
## ' @author m082166
addStatistic <- function(rows, stat, startRow, whichStat, colSize, outputTypes, isDate, digits,
						  nsmall, pctNSmall, includeTotal)
{
	numStats <- length (stat)
	if(!includeTotal && (numStats > 1))
		numStats <- numStats - 1
	for (theStat in seq_len (numStats)) {
		info <- stat[[theStat]]
		curRow <- startRow
		len <- NROW(info)
		if(is.data.frame(info) || is.matrix(info)) {	# Have a multi item, like Male / Female
			for (i in 1:len) {
				data <- info[i, ]
				isNum <- is.null(ncol(data))
				if(!isNum && (ncol(data) == 2)) {	# Have a two item result
					cell <- format.two(info[i, 1], info[i, 2], colSize, outputTypes, whichStat,
										isDate, digits, nsmall, pctNSmall)
				}
				else if(!isNum && (ncol(data) == 3)) {	# Have a three item result
					cell <- format.three(info[i, 1], info[i, 2], info[i, 3], colSize,
										 outputTypes, whichStat, isDate, digits, nsmall)
				}
				else {
					cell <- format.other(data, colSize, outputTypes, whichStat, isDate, digits,
										 nsmall, pctNSmall)
				}

				rows <- addToRow(rows, curRow, cell)
				curRow <- curRow + 1
			}
		}
		else {
			if(len == 2) {
				cell <- format.two(info[1], info[2], colSize, outputTypes, whichStat, isDate,
									digits, nsmall, pctNSmall)
			}
			else if(len == 3) {
				cell <- format.three(info[1], info[2], info[3], colSize, outputTypes,
									 whichStat, isDate, digits, nsmall)
			}
			else {
				cell <- format.other(info, colSize, outputTypes, whichStat, isDate, digits, nsmall,
									 pctNSmall)
			}
			rows <- addToRow(rows, curRow, cell)
			curRow <- curRow + 1
		}
	}

	return(rows)
}


## ' getStartingLine
## '
## ' A row is defined as a series of non-empty strings, ended by an empty string or by the end of the
## ' vector.  This method finds the 0 based curRow'th row, and returns its location within rows
## '
## ' @param rows		Vector of strings to look through
## ' @param curRow	Current row to operate on, 0 based
## ' @return The location in rows (1 based) holding the beginning of "row" curRow,
## ' or the length of rows if rows doesn't hold that many "rows"
## '
## ' @author m082166
getStartingLine <- function(rows, curRow) {
	arrayLen <- length(rows)
	start <- 1
	while (curRow > 0) {
		while ((start <= arrayLen) && (nchar(rows[start]) > 0)) {
			start <- start + 1
		}

		start <- start + 1	# Skip over the blank string
		curRow <- curRow - 1
	}

	return(start)
}


## ' format.two
## '
## ' Format two number output, according to the output type specified, padded to fill to colSize
## '
## ' @param first			First number to display
## ' @param second		Second number to display
## ' @param colSize		Width of the output string
## ' @param outputTypes	Vector from which to pull output type
## ' @param whichStat		Which of the items in outputTypes to use
## ' @param isDate		If true, show data as dates, if false treat normally
## ' @param digits		Number of digits to round to when displaying percent or Other data
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @param pctNSmall		Minimum number of digits to the right of the decimal point to display for
## ' percent numbers
## ' @return String of length colSize
## '
## ' @author m082166
format.two <- function(first, second, colSize, outputTypes, whichStat, isDate, digits, nsmall,
						pctNSmall) {
	doList <- isListOut(outputTypes, whichStat)
	doRange <- isRange(outputTypes, whichStat)
	first <- myFormat(first, digits, nsmall, isDate)

	if(doRange || doList) {
		if(doRange) {
			sep <- " - "
		}
		else {	# Do list
			sep <- ", "
		}
		second <- myFormat(second, digits, nsmall, isDate)
	}
	else {
		doTrim <- TRUE
		doPct <- isPct(outputTypes, whichStat)
		if(doPct && !is.null(pctNSmall)) {
			nsmall = pctNSmall
			doTrim <- FALSE
		}
		pct <- getPct(doPct)
		second <- myFormat(second, digits, nsmall, isDate, doTrim = doTrim)
		second <- paste0("(", second, pct, ")")
		sep <- " "
	}

	return(pastePaddedStr(c(first, second), colSize, sep = sep, appendSep = TRUE))
}


## ' format.three
## '
## ' Format three number output, according to the output type specified, padded to fill to colSize
## '
## ' @param first			First number to display
## ' @param second		Second number to display
## ' @param third     Third number to display
## ' @param colSize		Width of the output string
## ' @param outputTypes	Vector from which to pull output type
## ' @param whichStat		Which of the items in outputTypes to use
## ' @param isDate		If true, show data as dates, if false treat normally
## ' @param digits		Number of digits to round to when displaying percent or Other data
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @return String of length colSize
## '
## ' @author m082166
format.three <- function(first, second, third, colSize, outputTypes, whichStat, isDate, digits, nsmall) {
	doMedList <- isMedListOut(outputTypes, whichStat)
	doMedRange <- isMedRange(outputTypes, whichStat)
#	first <- format(first, digits = digits)

#	if(is.null(nsmall)) {
#		nsmall <- max(0, digits - integerDigits(second))
#		second <- format(round(as.numeric(second), nsmall), nsmall = nsmall)
#		nsmall <- max(0, digits - integerDigits(third))
#		third <- format(round(as.numeric(third), nsmall), nsmall = nsmall)
#	}
#	else {
#		second <- format(round(as.numeric(second), nsmall), nsmall = nsmall)
#		third <- format(round(as.numeric(third), nsmall), nsmall = nsmall)
#	}

	first <- myFormat(first, digits, nsmall, isDate)
	second <- myFormat(second, digits, nsmall, isDate)
	third <- myFormat(third, digits, nsmall, isDate)
	if(doMedRange || doMedList) {
		second <- paste0(first, " (", second)
		third <- paste0(third, ")")
		if(doMedRange)
			sep <- " - "
		else	# Do list
			sep <- ", "
	}
	else {
		second <- paste0(first, ", ", second)	# Do nothing with third, it's fine
		sep <- ", "
	}

	return(pastePaddedStr(c(second, third), colSize, sep = sep, appendSep = TRUE))
}


## ' format.other
## '
## ' Format data output, according to the output type specified, padded to fill to colSize
## '
## ' @param data			Data to display, be it number or date
## ' @param colSize		Width of the output string
## ' @param outputTypes	Vector from which to pull output type
## ' @param whichStat		Which of the items in outputTypes to use
## ' @param isDate		If true, show data as dates, if false treat normally
## ' @param digits		Number of digits to round to when displaying percent or Other data
## ' @param nsmall		Minimum number of digits to the right of the decimal point to display
## ' for floating point numbers.  If NULL, use 'digits' to determine everything
## ' @param pctNSmall		Minimum number of digits to the right of the decimal point to display for
## ' percent numbers
## ' @return String of length colSize
## '
## ' @author m082166
format.other <- function(data, colSize, outputTypes, whichStat, isDate, digits, nsmall, pctNSmall) {
	doDate <- isDateOut(outputTypes, whichStat)
	doFloat <- isFloatOut(outputTypes, whichStat)

	if(doDate) {
          sep <- " "
	}
	else if(doFloat) {
          if(is.null(nsmall)) {
            ## Don't allow nsmall < 0   -JPS 9/11/15
            nsmall <- max(0, digits - integerDigits(data))
          }
          ## data <- format(round(as.numeric(data), nsmall), nsmall = nsmall)
          data <- myFormat(data, digits, nsmall, isDate)
          sep <- " "
	}
	else {
          if(isPct(outputTypes, whichStat) && !is.null(pctNSmall)) {
            nsmall = pctNSmall
          }
          ##	if(is.null(nsmall)) {
          ##		data <- format(data, digits = digits)
          ##	}
          ##	else {
          ##		data <- format(data, digits = digits, nsmall = nsmall)
          ##	}
          data <- myFormat(data, digits, nsmall, isDate)
          sep <- ", "
	}

	return(pastePaddedStr(c(data), colSize, sep = sep, appendSep = TRUE))
}


## ' getPct
## '
## ' Return the proper "pct" string for the specified stat. "\%" if it's percent data, "" if not
## '
## ' @param doPct Logical. If \code{TRUE}, returns "\%" else "".
## ' @return The appropriate string: "\%" or ""
## '
## ' @author m082166
getPct <- function(doPct) {
	if(doPct)
		return("%")

	return(pct <- "")
}


## ' typeTest
## '
## ' Returns TRUE if this stat's output type matches value
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @param value (see below)
## ' @return TRUE if outputTypes[whichStat] == value output, else FALSE
## '
## ' @author m082166
typeTest <- function(outputTypes, whichStat, value) {
	if(is.null(outputTypes) || (length(outputTypes) < whichStat))
		return(FALSE)

	return(!is.na(outputTypes[whichStat]) && (outputTypes[whichStat] == value))
}


## ' isPct
## '
## ' Returns TRUE if this stat's two valued data should be displayed as a count / percent A (B%)
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if percent output, else FALSE
## '
## ' @author m082166
isPct <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'percent'))
}


## ' isRange
## '
## ' Returns TRUE if this stat's two valued data should be displayed as a range (A - B)
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if range output, else FALSE
## '
## ' @author m082166
isRange <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'range'))
}


## ' isMedRange
## '
## ' Returns TRUE if this stat's two valued data should be displayed as a median range A (B - C)
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if median range output, else FALSE
## '
## ' @author m082166
isMedRange <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'medrange'))
}


## ' isListOut
## '
## ' Returns TRUE if this stat's data should be displayed as a list A[, B]*
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if list output, else FALSE
## '
## ' @author m082166
isListOut <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'list'))
}


## ' isMedListOut
## '
## ' Returns TRUE if this stat's data should be displayed as a median list A (B, C)
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if median list output, else FALSE
## '
## ' @author m082166
isMedListOut <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'medlist'))
}


## ' isFloatOut
## '
## ' Returns TRUE if this stat's data should be displayed as floating point number(s)
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if floating point output, else FALSE
## '
## ' @author m082166
isFloatOut <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'float'))
}


## ' isDateOut
## '
## ' Returns TRUE if this stat's data should be displayed as a date
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return FALSE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if date output, else FALSE
## '
## ' @author m082166
isDateOut <- function(outputTypes, whichStat) {
	return(typeTest(outputTypes, whichStat, 'date'))
}


## ' isOther
## '
## ' Returns TRUE if this stat's two valued data should be displayed as an "other" type A (B)
## '
## ' @param outputTypes	Vector from which to pull this information. If NULL / NA, will return TRUE
## ' @param whichStat		Which of the items in outputTypes to look at
## ' @return TRUE if other, else FALSE
## '
## ' @author m082166
isOther <- function(outputTypes, whichStat) {
	if(is.null(outputTypes) || (length(outputTypes) < whichStat))
		return(TRUE)

	return(is.na(outputTypes[whichStat]))
}


## ' Make a string of the form "name (N=count)"
## '
## ' @param name	The label that's getting a count (i.e. "High")
## ' @param count	The count that goes with this label
## ' @return String of the form "name (N=count)"
## '
## ' @author m082166
makeCountHeader <- function(name, count) {
  return(paste0(name, " (N=", count, ")"))
}
