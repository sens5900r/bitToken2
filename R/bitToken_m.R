#' Tokenize text data in a data frame (Multicore)
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
#' @param num_cores The number of cores to use for parallel processing.
#' By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
#' However, the number of cores used is limited to half of the total available cores.
#
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
bitToken_m <- function(data, text_column, filter_var = NULL, filter_vals = NULL, lengths = FALSE, use_p = TRUE, num_cores = parallel::detectCores()) {
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }

  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  if (!is.character(data[[text_column]]) && !is.factor(data[[text_column]])) {
    stop(paste("Error: '", text_column, "' does not contain text data. Please provide a column that contains character or factor data.", sep = ""))
  }

  num_cores <- min(num_cores, parallel::detectCores() / 2)

  tokenize_text <- function(text) stringr::str_split(text, "\\s+")

  if (use_p) {
    tokens <- parallel::mclapply(data[[text_column]], tokenize_text, mc.cores = num_cores)
  } else {
    tokens <- lapply(data[[text_column]], tokenize_text)
  }

  tokens <- unlist(tokens, recursive = FALSE)

  if (!is.null(filter_var) & !is.null(filter_vals)) {
    tokens <- tokens[names(tokens) %in% filter_vals]
  }

  if (lengths) {
    lengths <- as.numeric(lengths(tokens))
    return(lengths)
  } else {
    return(tokens)
  }
}
