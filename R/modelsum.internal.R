## Purpose: internal functions (and methods) for tableby function
## Authors: Jason Sinnwell, Beth Atkinson
## Created: 9/4/2015

## Helper functions for modelsum:  merge, subset, and labels (work like names)

#' Helper functions for modelsum
#'
#' A set of helper functions for \code{\link{modelsum}}.
#'
#' @param object A \code{data.frame} resulting form evaluating \code{modelsum} formula.
#' @param ... Other arguments, or a vector of indices for extracting.
#' @param x,y A \code{modelsum} object.
#' @param i A vector to index \code{x} with: either names of variables, a numeric vector, or a logical vector of appropriate length.
#' @param value A list of new labels.
#' @return \code{na.modelsum} returns a subsetted version of \code{object} (with attributes).
#' @name modelsum.internal
NULL
#> NULL

starts_with <- function(x, prefix)
{
  substring(x, 1, nchar(prefix)) == prefix # this function is optimized in R >= 3.3 (startsWith)
}

join_formula <- function(x, y)
{
  x <- stats::formula(x)
  if(is.null(y)) return(x)
  y <- stats::formula(y)
  stopifnot(length(x) == 3 && length(y) == 2)
  x[[3]] <- call("+", x[[3]], y[[2]])
  x
}

#' @rdname modelsum.internal
#' @export
na.modelsum <- function (object, ...) {
    omit <- is.na(object[[1]])
    xx <- object[!omit, , drop = FALSE]
    if (any(omit > 0L)) {
        temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
        attr(xx, "na.action") <- set_attr(temp, "class", "omit")
    }
    xx
}

##standardized beta function (for gaussian stat)
lm.beta  <- function (MOD) {
    b <- stats::coef(MOD)[-1]
    sx <- rep(NA,length(b))
    b.idx <- 1
    for(k in 2:ncol(MOD$model)) {
      ## skip factors and char variables,
      ## psplines consider doing sx, but need a second for loop for the ncol of those
      if(any(class(MOD$model[,k]) %in% c("character","factor", "pspline"))) {
        b.idx <- b.idx + ifelse(is.null(ncol(MOD$model[,k])), length(unique(MOD$model[,k]))-1, ncol(MOD$model[,k]))
        ## skip as many elements of beta as there are N.levels-1 of categorical variables
      } else {
        sx[b.idx] <- stats::sd(as.double(MOD$model[,k]),na.rm=TRUE)
        b.idx <- b.idx + 1
      }
    }
    sy <- stats::sd(as.double(MOD$model[,1]),na.rm=TRUE)
    beta <- c(NA,round(b * sx/sy,3))
    return(beta)
}

## subset a modelsum object;
## syntax of usage: newtb <- tbObj[1:2]
## x here is the tableby object
## index is in '...', and allows only 1 vector of integer indices
## in future, maybe allow subsetting by names
#' @rdname modelsum.internal
#' @export
"[.modelsum" <- function(x, i) {
  if(missing(i)) return(x)
  newx <- x

  if(is.character(i) && any(i %nin% names(x$fits)))
  {
    tmp <- paste0(i[i %nin% names(x$fits)], collapse = ", ")
    warning(paste0("Some indices not found in modelsum object: ", tmp))
    i <- i[i %in% names(x$fits)]
  } else if(is.numeric(i) && any(i %nin% seq_along(x$fits)))
  {
    tmp <- paste0(i[i %nin% seq_along(x$fits)], collapse = ", ")
    warning(paste0("Some indices not found in modelsum object: ", tmp))
    i <- i[i %in% seq_along(x$fits)]
  } else if(is.logical(i) && length(i) != length(x$fits))
  {
    stop("Logical vector index not the right length.")
  }

  if(length(i) == 0 || anyNA(i)) stop("Indices must have nonzero length and no NAs.")

  newx$fits <- x$fits[i]
  return(newx)
}


## retrieve variable labels (y, x-vec) from tableby object
#' @rdname modelsum.internal
#' @export
labels.modelsum <- function(object, ...) {
  ##  get the formal labels from a tableby object's data variables
  ## y and x labels
  allLabels <- c(object$fits[[1]]$glance$endlabel, unlist(sapply(object$fits, function(obj) obj$label)))
  ##, sapply(object$x, function(obj) obj$label))
  names(allLabels) <- c(object$fits[[1]]$glance$endpoint, unlist(sapply(object$fits, function(obj) obj$xterm)))
  ## add on labels for adj vars
  if(!is.null(object$fits[[1]]$adjterms)) {
    nadj <- length(object$fits[[1]]$adjlabels)
    allLabels <- c(allLabels, object$fits[[1]]$adjlabels)
    names(allLabels)[(length(allLabels)-nadj+1):length(allLabels)] <- object$fits[[1]]$adjterms
  }

  return(allLabels)
}

## assign labels to modelsum object
#' @rdname modelsum.internal
#' @export
'labels<-.modelsum' <- function(x, value) {

  if(is.list(value)) value <- unlist(value)

  if(is.null(names(value))) {
    stop("labels for modelsum requires a named vector.")
  }
  vNames <- names(value)
  used.idx <- NULL
  for(k in seq_along(x$fits)) {
    v2x.idx <- match(vNames, x$fits[[k]]$xterm)
    x2v.idx <- match(x$fits[[k]]$xterm, vNames)
    if(sum(!is.na(x2v.idx)) > 0) {
      x$fits[[k]]$label[v2x.idx[!is.na(v2x.idx)]] <- value[x2v.idx]
      used.idx <- unique(c(used.idx, x2v.idx[!is.na(x2v.idx)]))
    }
    if(!is.null(x$fits[[k]]$adjterms)) {
      v2adj.idx <- match(vNames, x$fits[[k]]$adjterms)
      adj2v.idx <- match(x$fits[[k]]$adjterms,vNames)
      if(sum(!is.na(adj2v.idx)) > 0) {
        x$fits[[k]]$adjlabels[v2adj.idx[!is.na(v2adj.idx)]] <- value[adj2v.idx[!is.na(adj2v.idx)]]
        used.idx <- unique(c(used.idx, adj2v.idx[!is.na(adj2v.idx)]))
      }
    }
    y2v.idx <- match(x$fits[[k]]$glance$endpoint, vNames)
    if(!is.na(y2v.idx)) {
      x$fits[[k]]$glance$endlabel <- value[y2v.idx]
      used.idx <- unique(c(used.idx, y2v.idx))
    }
  }

  if(any(seq_along(value) %nin% used.idx)) {
    warning("Named value(s): ", paste(vNames[seq_along(value) %nin% used.idx], collapse=", "),
            " not matched in modelsum object")
  }

  ## return modelsum object with updated labels
  return(x)
}


## merge two tableby objects
## both must have same "by" variable and levels
## if some RHS variables have same names, keep both, the one in y add ".y"
#' @rdname modelsum.internal
#' @export
merge.modelsum <- function(x, y, ...) {

  newobj <- list(x, y)
  class(newobj) <- "modelsumList"
  return(newobj)
}

