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
#' }
#'
#' @export

bitToken_check <- function(data, text_column) {

  # check if the specified text_column is a valid column in the data frame
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  # check if the specified text_column contains only text data
  if (!is.character(data[[text_column]])) {
    return(paste("The column '", text_column, "' contains non-text data and cannot be tokenized with bitToken().", sep = ""))
  } else {
    return(paste("The column '", text_column, "' contains only text data and is suitable for tokenization with bitToken().", sep = ""))
  }
}
