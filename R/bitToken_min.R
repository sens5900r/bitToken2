#' Calculate the number of tokens in a given text column and find rows with at least a specified number of tokens
#'
#' This function takes in a dataframe, the name of the text column and the minimum number of tokens expected in the text column. It returns either the row indices or the actual row values based on the 'value' option
#'
#' @name bitToken_min
#' @param data a dataframe
#' @param col_name the name of the text column
#' @param min_tokens the minimum number of tokens expected in the text column (default = 3)
#' @param value a boolean value. If TRUE, the function returns the actual row values instead of the row indices (default = FALSE)
#'
#' @return A vector of either the row indices or the actual row values based on the 'value' option
#'
#' @importFrom stringr str_split
#'
#' @examples
#' bitToken_min(chatGPT_news1, "title", min_tokens = 5, value = TRUE)
#'
#' @export
bitToken_min <- function(data, col_name, min_tokens = 3, value = FALSE) {
  tokens <- str_split(data[[col_name]], "\\s+")
  num_tokens <- lengths(tokens)
  rows_with_min_tokens <- which(num_tokens >= min_tokens)

  if (value) {
    return(data[rows_with_min_tokens, col_name])
  } else {
    return(rows_with_min_tokens)
  }
}

