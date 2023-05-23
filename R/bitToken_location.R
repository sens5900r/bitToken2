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
bitToken_location <- function(data, text_column, pattern) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  # Tokenize the text column by splitting on whitespace
  tokens <- stringr::str_split(data[[text_column]], "\\s+")

  # Construct a vector marking the locations of the specified pattern
  positions <- lapply(tokens, function(token_list) {
    unname(sapply(token_list, function(token) {
      as.integer(grepl(pattern, token, fixed = TRUE))
    }))
  })

  return(positions)
}
