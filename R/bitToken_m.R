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
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#'
#' @return A list of token vectors or a vector of token lengths
#'
#' @examples
#' \dontrun{
#' library(bitToken2)
#' data(chatGPT_news1)
#' tokens <- bitToken_m(chatGPT_news1, "title", use_p = FALSE)
#' head(tokens)
#' token_lengths <- bitToken_m(chatGPT_news1, "title", lengths=TRUE, use_p = FALSE)
#' }
#'
#' @export
#'
#' @import dplyr
#' @import rlang
#' @import stringr
#'
#' @keywords tokenizing text
#' @seealso \code{\link[stringr]{str_split}} for more information on string splitting
bitToken_m <- function(data, text_column, filter_var = NULL, filter_vals = NULL, lengths = FALSE, use_p = TRUE) {
  # check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }

  # check that text_column is a valid column name in the data frame
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  # check that text_column contains text data
  if (!is.character(data[[text_column]]) && !is.factor(data[[text_column]])) {
    stop(paste("Error: '", text_column, "' does not contain text data. Please provide a column that contains character or factor data.", sep = ""))
  }

  if (!is.null(filter_var) & !is.null(filter_vals)) {
    filter_expr <- rlang::expr(!!rlang::sym(filter_var) %in% !!filter_vals)
    data <- dplyr::filter(data, !!filter_expr)
  }

  # Get the number of available cores
  num_cores <- parallel::detectCores()

  # Limit the number of cores to a maximum of 8
  num_cores <- min(num_cores, 8)

  # Function to tokenize text
  tokenize_text <- function(text) stringr::str_split(text, "\\s+")

  if (use_p) {
    # Tokenize the text column by splitting on whitespace in parallel
    tokens <- parallel::mclapply(data[[text_column]], tokenize_text, mc.cores = num_cores)
  } else {
    # Tokenize the text column by splitting on whitespace without parallel processing
    tokens <- lapply(data[[text_column]], tokenize_text)
  }

  # Flatten the nested list
  tokens <- unlist(tokens, recursive = FALSE)

  # return the tokens or lengths
  if (lengths) {
    lengths <- as.numeric(lengths(tokens))
    return(lengths)
  } else {
    return(tokens)
  }
}
