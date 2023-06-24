#' Check if a dataset is relevant for tokenizing with bitToken()
#'
#' This function checks if a dataset is relevant for tokenizing with bitToken() by ensuring that the specified text column contains only text data.
#'
#' @param data A data frame to check for tokenization
#' @param text_column The name of the column in \code{data} to check for tokenization
#'
#' @return A message indicating whether the specified text column contains only text data, or an error message if the column contains non-text data.
#'
#' @examples
#' \dontrun{
#' library(bitToken2)
#' data(mtcars)
#' bitToken_check(mtcars, "hp")
#' data(chatGPT_news1)
#' bitToken_check(chatGPT_news1, "title")
#' }
#'
#' @export
bitToken_check <- function(data, text_column) {
  # check if the data is a data frame
  if (is.character(data)) {
    warning(paste("Error: The 'data' argument must be an unquoted name of a data frame in the Global Environment. Please use the format bitToken_check(dataframe, \"column_name\")."))
    return()
  }
  
  # check if the text_column is a character
  if (!is.character(text_column)) {
    warning(paste("Error: The 'text_column' argument must be a quoted name of a column in the 'data' data frame. Please use the format bitToken_check(dataframe, \"column_name\")."))
    return()
  }
  
  # check if the specified text_column is a valid column in the data frame
  if (!text_column %in% names(data)) {
    warning(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
    return()
  }
  
  # check if the specified text_column contains only text data
  if (!is.character(data[[text_column]])) {
    return(paste("The column '", text_column, "' contains non-text data and cannot be tokenized with bitToken().", sep = ""))
  } else {
    return(paste("The column '", text_column, "' contains only text data and is suitable for tokenization with bitToken().", sep = ""))
  }
}
