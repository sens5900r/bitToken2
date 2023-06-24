#' Get Tokens in Descending Order by Frequency
#'
#' Extracts the specified token number from a column of text strings and returns a vector
#' of the tokens in descending order by their frequency.
#'
#' @param data A data frame containing the text column.
#' @param text_column The name of the column containing the text strings.
#' @param token_num The index of the token to extract (starting from 1).
#'
#' @return A data frame with two columns: "Token" and "Frequency", sorted by "Frequency" in descending order.
#'
#' @importFrom data.table data.table := .N setorder
#' @importFrom stringi stri_split_fixed
#'
#' @examples
#' bitToken_order(chatGPT_news1, "title", 1)
#'
#' @export
bitToken_order <- function(data, text_column, token_num) {
  require(data.table)
  require(stringi)
  
  data <- data.table(data) # Convert data to data.table
  
  # Split the text_column into tokens and select the specified token_num
  token_split <- stringi::stri_split_fixed(data[[text_column]], " ")
  data[, Token := sapply(token_split, function(x) if(length(x) >= token_num) x[token_num] else NA)]
  
  # Count the frequency of each token and convert to data.table
  token_freq <- data[, .(Frequency = .N), by = Token]
  
  # Remove rows where Token is NA
  token_freq <- token_freq[!is.na(Token), ]
  
  # Order the table by Frequency in descending order
  token_freq <- setorder(token_freq, -Frequency)
  
  return(head(token_freq, 10)) # Return top 10 tokens
}
