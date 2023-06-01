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
#' @import dplyr
#' @import rlang
#' @import stringr
#'
#' @keywords tokenizing text
#' @seealso \code{\link[stringr]{str_split}} for more information on string splitting
bitToken <- function(data, text_column, filter_var = NULL, filter_vals = NULL, lengths = FALSE) {
  # check that data is a data frame
  stopifnot(is.data.frame(data))

  # check that text_column is a valid column name in the data frame
  stopifnot(text_column %in% names(data))

  # check that text_column contains text data
  stopifnot(is.character(data[[text_column]]) || is.factor(data[[text_column]]))

  if (!is.null(filter_var) & !is.null(filter_vals)) {
    data <- dplyr::filter(data, !!rlang::sym(filter_var) %in% !!filter_vals)
  }

  # tokenize the text column by splitting on whitespace
  tokens <- stringr::str_split(data[[text_column]], "\\s+")

  # return the tokens or lengths
  if (lengths) {
    lengths <- as.numeric(lengths(tokens))
    return(lengths)
  } else {
    return(tokens)
  }
}
