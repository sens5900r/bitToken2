#' Calculate the number of tokens in a given text column and find rows with at least a specified number of tokens and not more than a maximum number
#'
#' This function takes in a dataframe, the name of the text column, the minimum number of tokens and maximum number of tokens expected in the text column. It returns either the row indices or the actual row values based on the 'value' option
#'
#' @name bitToken_min
#' @param data a dataframe
#' @param col_name the name of the text column
#' @param min_tokens the minimum number of tokens expected in the text column (default = 3)
#' @param max_tokens the maximum number of tokens expected in the text column (default = NULL)
#' @param value a boolean value. If TRUE, the function returns the actual row values instead of the row indices (default = FALSE)
#'
#' @return A vector of either the row indices or the actual row values based on the 'value' option
#'
#' @importFrom stringr str_split
#'
#' @examples
#' tokens5_to_7 <- bitToken_min(chatGPT_news1, "title", min_tokens = 5, max_tokens = 7, value = TRUE)
#'
#' @export
bitToken_min <- function(data, col_name, min_tokens = 3, max_tokens = NULL, value = FALSE) {
  tokens <- str_split(data[[col_name]], "\\s+")
  num_tokens <- lengths(tokens)

  if (is.null(max_tokens)) {
    rows_with_min_tokens <- which(num_tokens >= min_tokens)
  } else {
    if (max_tokens < min_tokens) {
      stop("max_tokens must be greater than or equal to min_tokens.")
    }
    rows_with_min_tokens <- which(num_tokens >= min_tokens & num_tokens <= max_tokens)
  }

  if (value) {
    return(data[rows_with_min_tokens, col_name])
  } else {
    return(rows_with_min_tokens)
  }
}
