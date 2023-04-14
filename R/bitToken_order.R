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
#' @importFrom stringr str_split
#' @importFrom dplyr group_by arrange summarise
#'
#' @examples
#' bitToken_order(chatGPT_news1, "title", 1)
#'
#' @export
bitToken_order <- function(data, text_column, token_num) {
  tokens <- stringr::str_split(data[[text_column]], "\\s+") # split the titles into tokens
  first_token <- sapply(tokens, function(x) x[token_num]) # get the specified token from each title
  token_freq <- table(first_token) %>% as.data.frame() # count the frequency of each token and convert to data frame
  names(token_freq) <- c("Token", "Frequency") # rename the columns
  return(token_freq %>% dplyr::group_by(Token = .data$Token) %>% dplyr::summarise(Frequency = sum(.data$Frequency)) %>% dplyr::arrange(desc(.data$Frequency))) # group by token and sum the frequency, arrange by frequency in descending order
}
