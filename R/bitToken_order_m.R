#' Get Tokens in Descending Order by Frequency
#'
#' Extracts the specified token number from a column of text strings and returns a vector
#' of the tokens in descending order by their frequency.
#'
#' @param data A data frame containing the text column.
#' @param text_column The name of the column containing the text strings.
#' @param token_num The index of the token to extract (starting from 1).
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#' @return A data frame with two columns: "Token" and "Frequency", sorted by "Frequency" in descending order.
#'
#' @importFrom stringr str_split
#' @importFrom dplyr group_by arrange summarise
#'
#' @examples
#' bitToken_order_m(chatGPT_news1, "title", 1, use_p = FALSE)
#'
#' @export
bitToken_order_m <- function(data, text_column, token_num, use_p = TRUE) {
  # Get the number of available cores
  num_cores <- parallel::detectCores()

  # Limit the number of cores to a maximum of 8
  num_cores <- min(num_cores, 8)

  # Function to extract tokens
  extract_tokens <- function(df, col) {
    strings <- df[[col]]
    tokens <- stringr::str_split(strings, "\\s+")
    sapply(tokens, function(x) x[token_num])
  }

  if (use_p) {
    # split the data frame across multiple cores
    data_splits <- split(data, rep(1:num_cores, each = ceiling(nrow(data) / num_cores))[1:nrow(data)])

    # apply stringr::str_split and sapply to each split in parallel
    tokens <- parallel::mclapply(data_splits, extract_tokens, col = text_column, mc.cores = num_cores)
  } else {
    # Extract tokens without parallel processing
    tokens <- extract_tokens(data, col = text_column)
  }

  # combine the token vectors from each core and count the frequency of each token
  tokens <- unlist(tokens)
  token_freq <- table(tokens) %>% as.data.frame()

  # rename the columns and return the data frame
  names(token_freq) <- c("Token", "Frequency")
  return(token_freq %>% dplyr::arrange(desc(.data$Frequency)))
}
