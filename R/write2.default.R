#' @rdname write2
#' @export
write2.default <- function(object, file, FUN = NULL, ..., append. = FALSE, render. = TRUE, keep.md = !render., output_format = NULL)
{
  if(!is.character(file) || length(file) > 1) stop("'file' argument must be a single character string.")
  if(!is.logical(append.) || length(append.) > 1) stop("'append.' argument must be a single logical value.")
  if(!is.logical(render.) || length(render.) > 1) stop("'render.' argument must be a single logical value.")
  if(!is.logical(keep.md) || length(keep.md) > 1) stop("'keep.md' argument must be a single logical value.")

  if(is.null(FUN))
  {
    object <- verbatim(object)
    FUN <- print
  }
  FUN <- match.fun(FUN)
  
  if(is.character(output_format) && length(output_format) > 1)
  {
    warning("At this point, write2() only supports one output type.")
    output_format <- output_format[1]
  }
  
  
  output_format <- if(is.null(output_format) || identical(output_format, "html"))
  {
    rmarkdown::html_document
  } else if(identical(output_format, "pdf"))
  {
    rmarkdown::pdf_document
  } else if(identical(output_format, "word"))
  {
    rmarkdown::word_document
  } else output_format
  
  filename <- paste0(file, ".md")
  if(!append. || !file.exists(filename)) file.create(filename) # this will create a blank document when needed but allows the append. = TRUE case to work, too
  dots <- list(...)
  if(names(formals(FUN))[1] == "...") # this is when the FUN is, e.g., cat(). Any named arguments would still get cat'd, which we don't want
  {
    ARGS <- c(list(object), dots[names(dots) %in% names(formals(FUN))])
    utils::capture.output(do.call(FUN, ARGS), file = filename, append = append.)
  } else
  {
    utils::capture.output(FUN(object, ...), file = filename, append = append.)
  }
  
  
  if(render.)
  {
    render.args <- dots[names(dots) %in% names(formals(rmarkdown::render))]
    render.args$input <- filename
    render.args$output_file <- file
    
    # if output_format is a function, evaluate it with the ... arguments
    # otherwise, just return the character string or list which rmarkdown::render() will handle
    render.args$output_format <- if(is.function(output_format))
    {
      if("..." %in% names(formals(output_format)))
      {
        do.call(output_format, dots)
      } else do.call(output_format, dots[names(dots) %in% names(formals(output_format))])
    } else output_format
    
    do.call(rmarkdown::render, render.args)
  }
  
  # This short-circuits if they want to keep the .md file. Otherwise, file.remove() returns a logical about successful file removal
  if(!keep.md && !file.remove(filename)) warning("Something went wrong removing the temporary .md file.")
  
  invisible(object)
}


