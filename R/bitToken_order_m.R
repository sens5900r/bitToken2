#' Get Tokens in Descending Order by Frequency (Multicore)
#'
#' Extracts the specified token number from a column of text strings and returns a vector
#' of the tokens in descending order by their frequency.
#'
#' @param data A data frame containing the text column.
#' @param text_column The name of the column containing the text strings.
#' @param token_num The index of the token to extract (starting from 1).
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#' @param num_cores The number of cores to be used for parallel processing. The default is the half of the total number of cores available in the system.
#' @return A data frame with two columns: "Token" and "Frequency", sorted by "Frequency" in descending order.
#'
#' @importFrom parallel mclapply detectCores
#' @importFrom stringi stri_split_fixed
#' @importFrom data.table data.table := .N setorder
#'
#' @examples
#' bitToken_order_m(chatGPT_news1, "title", 1, use_p = FALSE)
#'
#' @export
bitToken_order_m <- function(data, text_column, token_num, use_p = TRUE, num_cores = parallel::detectCores() / 2) {
  require(parallel)
  require(stringi)
  require(data.table)
  
  # Limit the number of cores to a maximum of 8
  num_cores <- min(num_cores, parallel::detectCores() / 2)
  
  # Function to extract tokens
  extract_tokens <- function(df, col) {
    strings <- df[[col]]
    tokens <- stringi::stri_split_fixed(strings, " ")
    sapply(tokens, function(x) if(length(x) >= token_num) x[token_num] else NA)
  }
  
  if (use_p) {
    # Split the data frame across multiple cores
    data_splits <- split(data, rep(1:num_cores, each = ceiling(nrow(data) / num_cores))[1:nrow(data)])
    
    # Apply extract_tokens to each split in parallel
    tokens <- parallel::mclapply(data_splits, extract_tokens, col = text_column, mc.cores = num_cores)
  } else {
    # Extract tokens without parallel processing
    tokens <- extract_tokens(data, text_column)
  }
  
  # Combine the token vectors from each core
  tokens <- unlist(tokens)
  
  # Convert data to data.table
  data <- data.table(data)
  
  # Count the frequency of each token and convert to data.table
  data[, Token := tokens]
  token_freq <- data[, .(Frequency = .N), by = Token]
  
  # Remove rows where Token is NA
  token_freq <- token_freq[!is.na(Token), ]
  
  # Order the table by Frequency in descending order
  token_freq <- setorder(token_freq, -Frequency)
  
  return(head(token_freq, 10)) # Return top 10 tokens
}
