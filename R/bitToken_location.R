#' Locate specific pattern in a text column of a data frame
#'
#' This function locates a specific pattern in a text column of a data frame.
#' It returns a binary vector indicating the presence of the pattern in each token.
#'
#' @param data Data frame that contains the text column.
#' @param text_column Name of the column in the data frame that contains the text to search for the pattern.
#' @param pattern Pattern to locate in the text.
#' @return A list of vectors indicating the presence of the pattern in each token. The length of the vector is equal to the number of tokens in the corresponding row.
#'         Each element in the vector is either 0 (the pattern is not present) or 1 (the pattern is present).
#' @examples
#' data(chatGPT_news1, package="bitToken2")
#' bitToken_location(chatGPT_news1, "title", "COVID")
#' @export
#' @import data.table
#' @import stringi
bitToken_location <- function(data, text_column, pattern) {
  # check if data is a data frame or a data.table
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Invalid input: 'data' must be a data frame or a data.table.")
  }
  
  # check if text_column is a valid column
  if (!text_column %in% names(data)) {
    stop(paste("Invalid input: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  # check if text_column contains character data
  if (!is.character(data[[text_column]])) {
    stop(paste("Invalid input: '", text_column, "' must be a character column.", sep = ""))
  }
  
  # convert to data.table
  dt <- data.table::as.data.table(data)
  
  # Tokenize the text column by splitting on whitespace
  tokens <- stringi::stri_split_fixed(dt[[text_column]], " ")
  
  # Construct a vector marking the locations of the specified pattern
  positions <- lapply(tokens, function(token_list) {
    unname(sapply(token_list, function(token) {
      as.integer(grepl(pattern, token, fixed = TRUE))
    }))
  })
  
  return(positions)
}
