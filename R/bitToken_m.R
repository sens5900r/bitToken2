#' Tokenize text data in a data frame (Multicore)
#'
#' @param data A data frame containing the text data to tokenize
#' @param text_column The name of the column in \code{data} containing the text data to tokenize
#' @param filter_var The name of a column in \code{data} to filter on (optional)
#' @param filter_vals A vector of values to filter on in \code{filter_var} (optional)
#' @param lengths A logical value indicating whether to return the lengths of the tokens (default = FALSE)
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#' @param num_cores The number of cores to use for parallel processing.
#' @return A list of token vectors or a vector of token lengths
#'
#' @export
#' @import data.table stringi parallel
bitToken_m <- function(data, text_column, filter_var = NULL, filter_vals = NULL, lengths = FALSE, use_p = TRUE, num_cores = parallel::detectCores()) {
  # check that data is a data frame
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  
  # check that text_column is a valid column name in the data frame
  if (!(is.character(text_column) && text_column %in% names(data))) {
    stop("The 'text_column' argument must be the name of a column in the 'data' data frame.")
  }
  
  # convert to data.table
  data <- data.table::as.data.table(data)
  
  # check that text_column contains text data
  if (!(is.character(data[[text_column]]) || is.factor(data[[text_column]]))) {
    stop("The specified 'text_column' must contain character or factor data.")
  }
  
  # limit number of cores to half of total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)
  
  # check filter_var and filter_vals, if they are provided
  if (!is.null(filter_var)) {
    if (!is.character(filter_var) || !filter_var %in% names(data)) {
      stop("The 'filter_var' argument must be the name of a column in the 'data' data frame.")
    }
    
    if (!is.null(filter_vals)) {
      if (!is.vector(filter_vals)) {
        stop("The 'filter_vals' argument must be a vector.")
      }
      # filter using data.table syntax
      data <- data[get(filter_var) %in% filter_vals]
    }
  }
  
  if (use_p) {
    tokenize_text <- function(text) stringi::stri_split_regex(text, "\\s+")
    # parallel tokenize
    tokens <- parallel::mclapply(data[[text_column]], tokenize_text, mc.cores = num_cores)
    # Flatten the list of lists to a single list
    tokens <- lapply(tokens, unlist)
  } else {
    tokenize_text <- function(text) stringi::stri_split_boundaries(text, type = "word")
    # non-parallel tokenize
    tokens <- lapply(data[[text_column]], tokenize_text)
    # Flatten the list of lists to a single list
    tokens <- lapply(tokens, unlist)
  }
  
  # return the tokens or lengths
  if (lengths) {
    if (!is.logical(lengths)) {
      stop("The 'lengths' argument must be a logical value.")
    }
    lengths <- as.numeric(vapply(tokens, length, integer(1L)))
    return(lengths)
  } else {
    return(tokens)
  }
}