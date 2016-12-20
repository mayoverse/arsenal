########## Note from Ethan: if we want these doc pages, just replace all instances of "## '" with "#'"


## ' addToRow
## ' 
## ' A row is defined as a series of non-empty strings, ended by an empty string or by the end of the 
## ' vector.  This method finds the 0 based curRow'th row, and pastes the strings passed in to the 
## ' matching strings in the row.  If there are already more strings in the row than there are to be 
## ' added, the remaining strings are padded with spaces, to the length of the first added string.  
## ' If there are more strings to add than are currently in the row, strings padded to the length of 
## ' the first string in the row are added to the row, then added to<br>
## ' Examples:<br>
## ' Input row: {"foo", "bar"}, toAdd {"baz"} --> {"foo baz", "bar    "}<br>
## ' Input row: {"foo"}, toAdd {"bar", "baz"} --> {"foo bar", "    baz"}<br>
## ' 
## ' @param rows		Vector of strings to edit, and possibly add to
## ' @param curRow	Current row to operate on, 0 based
## ' @param toAdd		Vector of strings to add to the appropriate row
## ' @param sep		Separator to use when pasting together, defaults to " "
## ' @param padChar	Character to use when padding out because len row != len toAdd, defaults to ' '
## ' @return The updated rows
## ' 
## ' @author m082166
addToRow <- function(rows, curRow, toAdd, sep = " ", padChar = ' ') {
  numLines <- length(rows)
  curLine <- getStartingLine(rows, curRow)
  if (curLine > numLines) {
    return(rows)	# Nothing to do, bail
  }
  
  rowStartSize <- nchar(rows[curLine])
  toAddSize <- nchar(toAdd[1])
  whichAdd <- 1
  numAdd <- length(toAdd)
  
  while (curLine <= numLines) {
    line <- rows[curLine]
    if (nchar(line) == 0)
      break
    
    if (whichAdd > numAdd) {	# Pad out the line with spaces
      line <- paste(line, makePaddedStr("", toAddSize, padChar = padChar), sep = sep)
    }
    else {
      line <- paste(line, toAdd[whichAdd], sep = sep)
    }
    
    rows[curLine] <- line
    curLine <- curLine + 1
    whichAdd <- whichAdd + 1
  }
  
  # Do we have to add more lines to rows?
  remainingLines <- (numAdd - whichAdd) + 1	# whichAdd is one greater than # actually added
  if (remainingLines > 0) {
    newLines <- c()
    for (i in 1:remainingLines) {
      newLines <- c(newLines, makePaddedStr("", rowStartSize, padChar = padChar))
    }
    
    if (curLine > numLines) {
      rows <- c(rows, newLines)
    }
    else {
      rows <- c(rows[1:(curLine - 1)], newLines, rows[curLine:numLines])
    }
    
    while (whichAdd <= numAdd) {
      line <- rows[curLine]
      line <- paste(line, toAdd[whichAdd], sep = sep)
      rows[curLine] <- line
      curLine <- curLine + 1
      whichAdd <- whichAdd + 1
    }
  }
  
  return(rows)
}


## ' integerDigits
## ' 
## ' Compute the number of integer digits (i.e. significant digits to the left of the decimal place) 
## ' a positive number has
## ' 
## ' @param aNumber	The number to do the calculations for
## ' @return An integer from 0 up, 0 if input isn't a number
## ' 
## ' @author m082166
integerDigits <- function (aNumber) {
       aNumber <- as.numeric(aNumber)
       numDigits <- 0
       if (!is.na(aNumber) && aNumber <0) {
              aNumber <- -aNumber;
              numDigits <- 1;      # Account for the minus sign
       }
       if (is.na(aNumber) || (aNumber <1) | is.infinite(aNumber))
              return(0)
       
       while (aNumber >=1) {
              aNumber <- aNumber /10
              numDigits <- numDigits +1
       }
       return(numDigits)
}


## ' makeTitleCell
## ' 
## ' Return an array of the lines needed to make the label cell, given the data in element, 
## ' taking into account the maximum allowed width specified by colSize, 
## ' which must be >= 4 + length of the name of element
## ' 
## ' @param element		List to get information from, whose first item must be the statistics
## ' @param colSize		Width to pad the output to
## ' @param translations	The List to use for conversion of labels
## ' @param boldMark		String to use to mark something as bold
## ' @param indentStr		String to use to indent something one space
## ' @param collapse ...?
## ' @return Vector holding the strings necessary to represent the rows of element, 
## ' each row separated by a blank string
## ' 
## ' @author m082166
makeTitleCell <- function(element, colSize, translations, boldMark, indentStr, collapse) {
  label <- paste0(boldMark, lookupHumanTitle(element$name, translations), boldMark)
  theCell <- c(makePaddedStr(label, colSize))
  
  if (collapse)	# If true, we're done here, nothing more to add
    return(theCell)
  
  statistics <- element$stats	# GTD 10/14/15 was statistics <- element[[1]]
  theNames <- names(statistics)
  numStats <- length(statistics)
  
  for (i in seq_len(numStats)) {
    statistic <- statistics[[i]]
    subRows <- rownames(statistic[[1]])
    if (length(subRows > 0)) {
      for (label in subRows) {
        label <- lookupHumanTitle(label, translations)
        # Add blank line to mark new row, then label line(s)
        theCell <- c(theCell, "", makeIndentedStr(label, colSize, indentStr = indentStr))
      }
    }
    else {
      label <- lookupHumanTitle(theNames[[i]], translations)
      # Add blank line to mark new row, then label line(s)
      theCell <- c(theCell, "", makeIndentedStr(label, colSize, indentStr = indentStr))
    }
  }
  
  return(theCell)
}


## ' makeHeader
## ' 
## ' Make the Pandoc format header for the table specified by group
## ' 
## ' @param group	Data we're making the table about
## ' @param minColSize Minimum size of the first column (which will hold label info for a row)
## ' @param includeTotal TRUE if should include last pre-pValue column, FALSE if shouldn't
## ' @param hasPValue	TRUE if should have column for p-values, FALSE if shouldn't
## ' @param pValueTitle	Title for pValue, only matters if hasPValue is TRUE
## ' @param leftJustify	If TRUE, will left justify each column, defaults to FALSE 
## ' @param rightJustify If TRUE, will right justify each column, defaults to FALSE 
## ' When both leftJustify and rightJustify are FALSE, columns are centered
## ' @param labelSize Relative size difference between label column and other columns. 
## ' 	 Default is 1.2: label column ~20\% bigger than other columns
## ' @return List holding the lines of the header defined by group plus
## ' lineSize: the length of a full line, 
## ' firstColSize: the length of the first column, 
## ' colSize: the length of each other column, 
## ' header: The lines of the header
## ' The last element is the last line of the output, to go after the body of the output
## ' headers: Vector of the plain header titles for each column
## ' 
## ' @author m082166
makeHeader <- function(group, minColSize, includeTotal, hasPValue, pValueTitle, labelSize = 1.2, 
                        leftJustify = FALSE, rightJustify = FALSE) {
  headers <- makeHeaders(group, includeTotal, hasPValue, pValueTitle)
  size <- max(nchar(headers)) + 2	# Need one extra "-" on either side to center text
  if (size < 10)
    size = 10	# Minimum width for a column
  bigSize = round(size * labelSize)	# Want the first column ~ 20% larger than other columns
  if (bigSize < minColSize) {
    bigSize <- minColSize
    size <- round(bigSize / labelSize)
  }
  fullSize = bigSize + ((size + 1) * length(headers))
  outsideLine <- makeDashStr(fullSize)
  
  header <- outsideLine
  #First Line
  head <- makeDashStr(bigSize, theChar = ' ')
  
  for (cellH in headers) {	# Want it to insert space as separator
    head <- paste(head, makeCellHeader(cellH, size, leftJustify, rightJustify))
  }
  
  header <- c(header, head)
  #Second line
  head <- makeDashStr(bigSize)
  
  for (cellH in headers) {
    head <- paste(head, makeDashStr(size))	# Want it to insert space as separator
  }
  
  header <- c(header, head)
  header <- c(header, outsideLine)
  
  # Build a named List with the data to return
  header <- list(lineSize = fullSize, firstColSize = bigSize, colSize = size, header = header, 
                 headers = headers)
  
  return(header)
}


## ' makeHeaders
## ' 
## ' Make the unpadded header for each column other than the label column
## ' 
## ' @param group			The data that will be turned into a table
## ' @param includeTotal	TRUE if should include last pre-pValue column, FALSE if shouldn't
## ' @param hasPValue		TRUE if should have column for p-values, FALSE if shouldn't
## ' @param pValueTitle	Title for pValue, only matters if hasPValue is TRUE
## ' @return A Vector of the column headers, given the data in group, 
## ' skipping the first (blank, label) header
## ' 
## ' @author m082166
makeHeaders <- function(group, includeTotal, hasPValue, pValueTitle) {
  element <- group[[1]]
  headers <- c()
  len <- length(element)
  
  if (!includeTotal && (len > 1))
    len <- len - 1
  theNames <- names(element)
  for (i in seq_len(len)) {
    headers <- c(headers, makeCountHeader(theNames[i], element[i]))
  }
  
  if (hasPValue) {
    headers <- c(headers, pValueTitle)
  }
  
  return(headers)
}


## ' makeCellHeader
## ' 
## ' Create a string holding the complete header for a cell, of length size (which must be >= length 
## ' of text).  Defaults to center justified text , which requires that size >= length of text + 2
## ' Input is not currently validated, caller responsible for setting values correctly
## ' 
## ' @param text			The text of the header
## ' @param size			The size the header must be padded to
## ' @param leftJustify	If TRUE, will left justify each column, defaults to FALSE 
## ' @param rightJustify	If TRUE, will right justify each column, defaults to FALSE 
## ' When both leftJustify and rightJustify are FALSE, columns are centered
## ' @return String of length size
## ' 
## ' @author m082166
makeCellHeader <- function(text, size, leftJustify = FALSE, rightJustify = FALSE) {
  neededSpaces <- size - nchar(text)
  
  if (rightJustify) {
    out <- paste0(makeDashStr(neededSpaces, ' '), text)
  }
  else {
    if (leftJustify) {
      out <- ""
    }
    else {
      out <- " "
      neededSpaces <- neededSpaces - 1
    }
    
    out <- paste0(out, text, makeDashStr(neededSpaces, ' '))
  }
  
  return(out)
}


## ' makeIndentedStr
## ' 
## ' Make a string consisting of indent copies of indentStr followed by the starting string, 
## ' all this broken across as many lines as need to so each line is length size or less, 
## ' each line followed by however many instances of the repeated character, which defaults to ' ', 
## ' are needed to pad out each line to the requested length
## ' 
## ' @param startStr	The text that will be indented and displayed
## ' @param size		Padded width of resulting strings
## ' @param padChar	Character to pad out strings to length size, defaults to ' ', 
## ' will cause problems if not nchar 1
## ' @param indent	Number of spaces to indent startStr, defaults to 3
## ' @param indentStr	String to use to indent something one space, defaults to "&nbsp;" 
## ' (HTML non-breaking space)
## ' @return Vector of one or more strings holding the Pandoc code required to display the indented 
## ' string
## ' 
## ' @author m082166
makeIndentedStr <- function(startStr, size, padChar = ' ', indent = 3, indentStr = "&nbsp;") {
  indentLen = nchar(indentStr)
  curSize <- 0
  results <- c()
  curStr <- ""
  
  # First add the indent
  for (i in 1:indent) {
    curSize <- curSize + indentLen
    if (curSize <= size)
      curStr <- paste0(curStr, indentStr)
    else {
      results <- c(results, makePaddedStr(curStr, size, padChar = padChar))
      curSize <- indentLen
      curStr <- indentStr
    }
  }
  
  # Special case a plain text indent
  if ((indentLen == 1) && (length(results) == 0) && ((curSize * 2) < size)) {
    remainingSize <- size - curSize
    hold <- makePaddedStr(startStr, remainingSize, padChar = padChar)
    for (outStr in hold) {
      results <- c(results, paste0(curStr, outStr))
    }
    
    return(results)
  }
  
  # Close off indents as their own line
  results <- c(results, makePaddedStr(curStr, size, padChar = padChar))
  
  # Now add the string
  results <- c(results, makePaddedStr(startStr, size, padChar = padChar))
  
  return(results)
}


## ' pastePaddedStr
## ' 
## ' Make one or more strings consisting of the strings in strArray, separated by sep, 
## ' followed by however many instances of the repeated character are needed to pad out the results 
## ' to strings of the requested length
## ' 
## ' @param strArray	Vector of strings to paste together
## ' @param size		Size of each output string. If size < length of any of the strings in strArray, 
## ' strings will be split by makePaddedStr
## ' @param sep		Separator for paste, defaults to " "
## ' @param padChar	Character to pad out strings to length size, defaults to ' ', 
## ' will cause problems if not nchar 1
## ' @param appendSep	If TRUE, and sep and padChar are different, will make sure that n - 1 sep 
## ' appear in output (where n = length (strArray)), defaults to FALSE
## ' @return Vector of strings of length size
## ' 
## ' @author m082166
pastePaddedStr <- function(strArray, size, sep = " ", padChar = ' ', appendSep = FALSE) {
  if (appendSep)
    appendSep = (sep != padChar)
  
  out <- ''
  curSize <- 0
  results <- c()
  sepLen = nchar(sep)
  
  for (aStr in strArray) {
    strLen <- nchar(aStr)
    if (curSize == 0) {
      out <- aStr
      curSize <- strLen
    }
    else {
      test <- curSize + sepLen
      curSize <- test + strLen
      if (curSize <= size) {
        out <- paste0(out, sep, aStr)
      }
      else {
        addSep <- appendSep
        if (appendSep && (test <= size)) {	# Can we add sep to the end without problem?
          out <- paste0(out, sep)
          addSep <- FALSE
        }
        results <- c(results, makePaddedStr(out, size, padChar = padChar))
        if (addSep) {
          curSize <- strLen + sepLen
          if (curSize > size) {	# No choice, have to put separator on own line
            results <- c(results, makePaddedStr(sep, size, padChar = padChar))
            curSize <- strLen
            out <- aStr
          }
          else {
            out <- paste0(sep, aStr)
          }
        }
        else {
          curSize <- strLen
          out <- aStr
        }
      }
    }
  }
  
  results <- c(results, makePaddedStr(out, size, padChar = padChar))
  
  return(results)
}


## ' addNumberToEnd
## ' 
## ' Make a string starting with the starting string, and ending with the passed in number, 
## ' with however many instances of the repeated character are needed to pad out the string to the 
## ' requested length
## ' 
## ' @param startStr	Beginning string
## ' @param addNum	Number to put at the end, if NA, or not a number, will just pad to end
## ' @param size		How big to make the string
## ' @param  digits	Number of digits to give the number when formatting it, defaults to 5
## ' @param  padChar	What to use when padding the string, defaults to ' ', may break if length != 1
## ' @param  endText	Text to add after the number, defaults to ""
## ' @return String of length size, starting with startStr, ending with addNum
## ' 
## ' @author m082166
addNumberToEnd <- function(startStr, addNum, size, digits = 5, padChar = ' ', endText = "") {
  addNum <- as.numeric(addNum)
  if (is.na(addNum))
    return(makePaddedStr(startStr, size, padChar = padChar))
  
  numStr <- paste0(makeLimitedNumber(addNum, digits), endText)
  out <- startStr
  padSize <- nchar(padChar)
  neededSpaces <- (size - nchar(startStr) - nchar(numStr) - padSize) / padSize
  if (neededSpaces >= 1) {
    for (i in 1:neededSpaces) {
      out <- paste0(out, padChar)
    }
  }
  
  return(paste(out, numStr, sep = padChar))
}



## ' makeLimitedNumber
## ' 
## ' Make a string with a number, or "< x", where X is minimum number to show
## ' 
## ' @param addNum	Number to use
## ' @param digits	Number of digits to give the number when formatting it
## ' @return String holding addNum, or "< 10^-digits"
## ' 
## ' @author m082166
makeLimitedNumber <- function(addNum, digits) {
  test <- 10^(-digits)
  if (test > addNum) {	# No rounding, it's a strictly less than test
    return (paste0("<", format(test, nsmall = digits, scientific = FALSE)))
  }
  else {
    return (format(round(addNum, digits), nsmall = digits, scientific = FALSE))
  }
}


## ' makePaddedStr
## ' 
## ' Make a string consisting of the starting string, followed by however many instances of the 
## ' repeated character are needed to pad out the string to the requested length.
## ' 
## ' If nchar (startStr) > size, will try to split startStr at reasonable points so smaller than size, 
## ' if that fails will simply break it at size length, & will then return a Vector of padded strings
## ' 
## ' @param startStr	String providing the text to get padded out
## ' @param size		Size to pad the string out to
## ' @param padChar	Character to pad out string to length size, defaults to ' ', 
## ' will cause problems if not length 1
## ' @return String of length size, or Vector of strings of length size, if nchar (startStr) > size
## ' 
## ' @author m082166
makePaddedStr <- function(startStr, size, padChar = ' ') {
  if (is.null(startStr))
    startStr <- ""
  out <- startStr
  neededSpaces <- size - nchar(startStr)
  if (neededSpaces > 0) {
    for (i in 1:neededSpaces) {
      out <- paste0(out, padChar)
    }
  }
  
  if (neededSpaces >= 0) {
    return(out)
  }
  
  # See if can split the string on a reasonable boundary character
  for (split in c(" ", "\t", "_", "-", "*", ".", ";", ":")) {
    results <- strsplit(startStr, split, fixed = TRUE)[[1]]
    if ((length(results) > 1) && (minStrLen(results) <= size))
      return(pastePaddedStr(results, size, sep = split, padChar = padChar, appendSep = TRUE))
  }
  
  len <- nchar(startStr)
  start <- 0
  results <- c()
  
  while ((len - start) > size) {
    results <- c(results, substr(startStr, start + 1, start + size))
    start <- start + size
  }
  
  return(c(results, makePaddedStr(substr(startStr, start + 1, len), size, padChar = padChar)))
}





## ' endsWithPad
## ' 
## ' Reports if a string ends (or starts) with one of the "boundary characters" that a padded string 
## ' could have been split on, including a space
## ' 
## ' @param testStr	String to test
## ' @param testEnd	If TRUE, will test last character, if FALSE will test first character 
## ' @return TRUE if has a "pad" / "boundary" character in tested place, else FALSE
## ' 
## ' @author m082166
endsWithPad <- function(testStr, testEnd = TRUE)
{
  len <- nchar (testStr)
  if (len == 0)
    return (FALSE)
  
  if (!testEnd)
    len <- 1	# Get the first character
  
  testChar <- substr (testStr, len, len)
  for (split in c (" ", "\t", "_", "-", "*", ".", ";", ":"))
  {
    if (split == testChar)
      return (TRUE)
  }
  
  return (FALSE)
}


## ' beginsWithPad
## ' 
## ' Reports if a string starts with one of the "boundary characters" that a padded string 
## ' could have been split on, including a space
## ' 
## ' @param testStr	String to test
## ' @return TRUE if starts with a "pad" / "boundary" character, else FALSE
## ' 
## ' @author m082166
beginsWithPad <- function(testStr)
{
  return (endsWithPad (testStr, testEnd = FALSE))
}





## ' makeCenteredStr
## ' 
## ' Make a string consisting of the starting string, surrounded by however many instances of padChar 
## ' are needed to pad out the string to the requested length.
## ' 
## ' If nchar (startStr) > size, will split startStr at whitespace so smaller than size, 
## ' and will then return a Vector of centered strings of decreasing length.<br>
## ' If there are lineSplit strings in startStr, will first be split into separate lines, and 
## ' each line will be centered separately 
## ' 
## ' @param startStr	String providing the text to get centered
## ' @param size		Size to pad the string out to
## ' @param padChar	Character to pad out string to length size, defaults to ' ', 
## ' will cause problems if not length 1
## ' @param lineSplit	String to split startStr into separate lines, defaults to "\\n"
## ' @param sizeLimit	If TRUE, will never output a line length > size, if FALSE will try to avoid 
## ' over-long lines, but a "word" longer than size will just be its own line
## ' @return String of length size, or Vector of strings of length size
## ' 
## ' @author m082166
makeCenteredStr <- function(startStr, size, padChar = ' ', lineSplit = "\n", sizeLimit = FALSE) {
  finalResults <- c()
  for (out in strsplit(startStr, lineSplit, fixed = TRUE)[[1]]) {
    neededSpaces <- size - nchar(out)
    if (neededSpaces > 0) {
      start <- ceiling(neededSpaces / 2)
      end <- neededSpaces - start
      out <- paste0(makeDashStr(start, " "), out, makeDashStr(end, " "))
    }
    
    if (neededSpaces >= 0) {
      finalResults <- c(finalResults, out)
      next
    }
    
    # See if can split the string on spaces
    results <- strsplit(out, " ", fixed = TRUE)[[1]]
    if ((length(results) > 1) && (minStrLen(results) <= size)) {
      finalResults <- c(finalResults, pasteCenteredStr(results, size, padChar = padChar))
      next
    }
    
    if (sizeLimit) {
      len <- nchar(out)
      start <- 0
      results <- c()
      
      while ((len - start) > size) {
        results <- c(results, substr(out, start + 1, start + size))
        start <- start + size
      }
      
      results <- c(results, makeCenteredStr(substr(out, start + 1, len), size, 
                                            padChar = padChar, lineSplit = lineSplit))
      finalResults <- c(finalResults, results)
    }
    else  {
      finalResults <- c(finalResults, out)
    }
  }
  
  return(finalResults)
}


## ' pasteCenteredStr
## ' 
## ' Make one or more strings consisting of the strings in strArray, separated by sep, 
## ' surrounded by however many instances of padChar are needed to pad out the results 
## ' to centered strings of the requested length.  The strings will be of roughly equal size, 
## ' with a bias for strings to be in decreasing size as we go forward
## ' 
## ' @param strArray	Vector of strings to paste together
## ' @param size		Size of each output string. If size < length of any of the strings in strArray, 
## ' strings will be split by makeCenteredStr
## ' @param sep		Separator for paste, defaults to " "
## ' @param padChar	Character to pad out strings to length size, defaults to ' ', 
## ' will cause problems if not nchar 1
## ' @return Vector of strings of length size
## ' 
## ' @author m082166
pasteCenteredStr <- function(strArray, size, sep = " ", padChar = ' ') {
  out <- ''
  curSize <- 0
  results <- c()
  sepLen = nchar(sep)
  totalLen <- sumStrLen(strArray) + (sepLen * (length(strArray) - 1))
  minNumLines <- ceiling(totalLen / size)
  baseLineLen <- ceiling(totalLen / minNumLines)
  
  for (aStr in strArray) {
    strLen <- nchar(aStr)
    if (curSize == 0) {
      out <- aStr
      curSize <- strLen
    }
    else {
      curSize <- curSize + sepLen + strLen
      if (curSize < baseLineLen) {
        out <- paste0(out, sep, aStr)
      }
      else {
        if (curSize <= size) {
          out <- paste0(out, sep, aStr)
          results <- c(results, makeCenteredStr(out, size, padChar = padChar))
          curSize <- 0
          out <- ''
        }
        else  {
          results <- c(results, makeCenteredStr(out, size, padChar = padChar))
          curSize <- strLen
          out <- aStr
        }
      }
    }
  }
  
  if (curSize > 0) {
    results <- c(results, makeCenteredStr(out, size, padChar = padChar))
  }
  
  return(results)
}



## ' myFormat
## ' 
## ' Format a number, adjusting nsmall as appropriate, and remove all trailing 0s, and a trailing "."
## ' 
## ' @param number	String to format
## ' @param digits	Number of digits to display for number.  Will be passed to format.
## ' If nsmall is NULL, will set to max(0, digits - number of integer digits in number)
## ' @param nsmall	Minimum number of non-zero digits to the right of the decimal point to display.
## ' Will trim off any ending 0s after decimal place
## ' @param isDate	Is it a Date?  If TRUE, return it as.character
## ' @param doTrim	If doTrim is false, won't trim, otherwise will.  Default is TRUE
## ' @return Resulting String, will not be empty unless number was empty
## ' 
## ' @author m082166
myFormat <- function(number, digits, nsmall, isDate = FALSE, doTrim = TRUE) {
  if (isDate)
    return(as.character (number))
  
  if (is.null(nsmall)) {
    nsmall <- max(0, digits - integerDigits(number))
  }
  
  if (doTrim)
    return(trimNumber(format(round(as.numeric(number), nsmall), digits = digits, nsmall = nsmall)))
  
  return(format(round(as.numeric(number), nsmall), digits = digits, nsmall = nsmall))
}


## ' trimNumber
## ' 
## ' Take a string representing a number, and remove all trailing 0s, and a trailing "."
## ' 
## ' @param number String to trim
## ' @return Resulting String, will not be empty unless number was empty
## ' 
## ' @author m082166
trimNumber <- function(number) {
  len <- maxStrLen(number)
  if (len <= 1)
    return(number)
  
  dotPos <- str_locate(number, fixed ("."))[1]
  if (is.na(dotPos))
    return(number)
  
  start <- len
  while ((len > 1) && (substr(number, len, len) == "0")) {
    len = len - 1
  }
  
  if (substr(number, len, len) == ".") {
    if (len > 1) {
      len = len - 1
    }
    else {	# All we have is "."
      return("0")
    }
  }
  
  if (len == start)
    return(number)
  
  return(substr(number, 1, len))
}


## ' makeDashStr
## ' 
## ' Make a string consisting of count instances of the repeated character
## ' 
## ' @param count		Size of the returned string
## ' @param theChar	Character to use when building the string, defaults to '-'
## ' @return String of length count
## ' 
## ' @author m082166
makeDashStr <- function(count, theChar = '-') {
  return(paste(replicate(count, theChar), collapse = ""))
}



## ' maxNameLen
## ' 
## ' Return the length of the longest string among the names of elements
## ' 
## ' @param elements		A List of Lists
## ' @param translations	The List to use for conversion of labels, so can use the proper name length
## ' @return The nchar length of the longest name from element's sub-lists, 
## ' as translated via translations
## ' 
## ' @author m082166
maxNameLen <- function(elements, translations) {
  theMax = 0
  
  for (item in elements) {
    theMax <- max(theMax, nchar(lookupHumanTitle(names(item[[1]]), translations)))
  }
  
  return(theMax)
}


## ' maxStrLen
## ' 
## ' Return the length of the longest string in a Vector of strings
## ' 
## ' @param strings Vector of Strings
## ' @return The nchar length of the longest string in the Vector
## ' 
## ' @author m082166
maxStrLen <- function(strings) {
  return(max(0, nchar(strings)))
}


## ' minStrLen
## ' 
## ' Return the length of the shortest string in a Vector of strings
## ' 
## ' @param strings	Vector of Strings
## ' @return The nchar length of the shortest string in the Vector
## ' 
## ' @author m082166
minStrLen <- function(strings) {
  return(min(0, nchar(strings)))
}


## ' sumStrLen
## ' 
## ' Return the sum of the lengths of the strings in a Vector of strings
## ' 
## ' @param strings	Vector of Strings
## ' @return The sum of the nchar lengths of the strings in the Vector
## ' 
## ' @author m082166
sumStrLen <- function(strings) {
  return(sum(0, nchar(strings)))
}


## ' lookupHumanTitle
## ' 
## ' Take a string, and see if we have a human readable version of that string
## ' 
## ' @param label			The label to convert, or a vector of labels to convert
## ' @param translations	The List to use for conversion of labels, defaults to format.translations
## ' @return More human readable version of the label, if have one, else the passed in label.
## ' If was passed a Vector, will return a Vector of translations
## ' 
## ' @author m082166
lookupHumanTitle <- function(label, translations = format.translations) {
  if (length(label) == 1)	{	# Can have single string, or vector of strings
    humanText <- translations[[label]]
    if (!is.null(humanText))
      return(humanText)
    
    return(label)
  }
  
  results <- c()
  for (aLabel in label) {
    humanText <- translations[[aLabel]]
    if (!is.null(humanText)) {
      results <- c(results, humanText)
    }
    else {
      results <- c(results, aLabel)
    }
  }
  
  return(results)
}




## ' format.addTranslations
## ' 
## ' Add translations from machine produced labels to a human readable ones
## ' Get translations from object control parameters as well as what was passed in
## ' 
## ' @param object	The data defining the table to display, and its control parameters
## ' @param transList	List where name is the label in the output, and value is the label to display
## ' e.g. list (q1q3 = "Q1, Q3", medsurv = "Median Survival")
## ' @param baseList	List holding any default / starting translations
## ' @param elemCol	The column of object that holds the elements, defaults to "x"
## ' @param nameCol ...?
## ' @return Current translation list
## ' 
## ' @author m082166
format.addTranslations <- function(object, transList, baseList = format.translations, 
                                    elemCol = "x", nameCol = "name")
{
  elements <- object[[elemCol]]
  
  # First add all names with a label of the name capitalized
  #	theNames <- names(elements)
  #	for (name in theNames) {
  #		baseList[[name]] <- capitalizeWords(name)
  #	}
  
  # Now get every name that has an existing label, where label != name
  for (element in elements) {
    theLabels <- element$label
    theNames <- element[[nameCol]]
    numNames <- min (length (theNames), length (theLabels))
    
    for (i in seq_len (numNames)) {
      name <- theNames[i]
      label <- theLabels[i]
      if (name != label) {
        baseList[[name]] <- label
      }
    }
  }
  
  # Now get labels specified in the control parameter, which is a named list
  labels <- object$control$stats.labels
  theNames <- names (labels)
  for (name in theNames) {
    baseList[[name]] <- labels[[name]]
  }
  
  # Finally get any labels specified with the call to summary
  for (theName in names(transList)) {
    baseList[[theName]] <- transList[[theName]]
  }
  
  return(baseList)
}




## ' addTranslations
## ' 
## ' Add translations from machine produced labels to a human readable ones
## ' 
## ' @param translations	List to add to where name is the label in the output, and value is the 
## ' label to display, e.g. list (q1q3 = "Q1, Q3", medsurv = "Median Survival")
## ' @param machine	Machine produced labels
## ' @param human	Human readable versions
## ' @return Updated translation list
## ' 
## ' @author m082166
addTranslations <- function(translations, machine, human) {
  #	for (labs in machine) {
  #		translations[[labs]] <- human[labs]
  #	}
  
  transCount <- min(length(machine), length(human))
  
  for (i in seq_len(transCount)) {
    name <- machine[i]
    label <- human[i]
    if (name != label) {
      translations[[name]] <- label
    }
  }
  
  return(translations)
}



## ' capitalizeWords
## ' 
## ' Take a string holding one or more words, and capitalize each word
## ' 
## ' @param theStr	String to capitalize
## ' @return String with each word capitalized
## ' 
## ' @author m082166
capitalizeWords <- function(theStr) {
  strArry <- strsplit(theStr, " ")[[1]]
  return (paste0(toupper(substring(strArry, 1,1)), substring(strArry, 2), collapse = " "))
}



## ' setParam
## ' 
## ' Figure out which parameter value to use, and return it
## ' 
## ' @param value		The value to test.  If it passes the test, will use it. If not, will use default
## ' @param default	What to use if value isn't valid
## ' @param testNA	If TRUE, reject value if it's NA, if FALSE, reject value if it's NULL.  
## ' Defaults to TRUE
## ' @return Value, or default
## ' 
## ' @author m082166
setParam <- function(value, default, testNA = TRUE) {
  if (testNA) {
    if (is.na(value))
      return (default)
    return (value)
  }
  
  if (is.null(value))
    return (default)
  return (value)
}


## ' setParam3
## ' 
## ' Figure out which parameter value to use, and return it, three option case
## ' 
## ' @param value			The value to test.  If it passes the test, will use it. 
## ' If not, will use default
## ' @param default		What to use if value isn't valid
## ' @param finalDefault	What to use if default isn't valid
## ' @param testNA	If TRUE, reject value if it's NA, if FALSE, reject value if it's NULL.  
## ' Defaults to TRUE.  However, when testing "default", will test for NULL in all cases
## ' @return Value, or default
## ' 
## ' @author m082166
setParam3 <- function(value, default, finalDefault, testNA = TRUE) {
  if (testNA) {
    if (is.na(value)) {
      if (is.null(default)) {
        return (finalDefault)
      }
      return (default)
    }
    return (value)
  }
  
  if (is.null(value)) {
    if (is.null(default)) {
      return (finalDefault)
    }
    return (default)
  }
  return (value)
}


