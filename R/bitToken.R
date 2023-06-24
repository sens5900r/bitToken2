#' Tokenize text data in a data frame
#'
#' This function tokenizes text data in a specified column of a data frame.
#' The resulting tokens are returned as a list or, optionally, as a vector of token lengths.
#' Filtering options are also available to allow the user to filter the data frame based on specific values.
#'
#' @param data A data frame containing the text data to tokenize
#' @param text_column The name of the column in \code{data} containing the text data to tokenize
#' @param filter_var The name of a column in \code{data} to filter on (optional)
#' @param filter_vals A vector of values to filter on in \code{filter_var} (optional)
#' @param lengths A logical value indicating whether to return the lengths of the tokens (default = FALSE)
#'
#' @return A list of token vectors or a vector of token lengths
#'
#' @examples
#' \dontrun{
#' library(bitToken2)
#' data(chatGPT_news1)
#' tokens <- bitToken(data = chatGPT_news1, text_column = "title")
#' head(tokens)
#' token_lengths <- bitToken(data = chatGPT_news1, text_column = "title", lengths=TRUE)
#' }
#'
#' @export
#'
#' @import data.table
#' @import stringi
bitToken <- function(data, text_column, filter_var = NULL, filter_vals = NULL, lengths = FALSE) {
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
  
  # tokenize the text column by splitting on whitespace
  tokens <- stringi::stri_split_regex(data[[text_column]], "\\s+")
  
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
