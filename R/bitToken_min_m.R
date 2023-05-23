#' Calculate the number of tokens in a given text column and find rows with at least a specified number of tokens (Multicore)
#'
#' This function takes in a dataframe, the name of the text column and the minimum number of tokens expected in the text column. It returns either the row indices or the actual row values based on the 'value' option
#'
#' @param data a dataframe
#' @param col_name the name of the text column
#' @param min_tokens the minimum number of tokens expected in the text column (default = 3)
#' @param value a boolean value. If TRUE, the function returns the actual row values instead of the row indices (default = FALSE)
#' @param use_p A logical value. If \code{TRUE}, the function will use parallel processing. Default is \code{TRUE}.
#' @param num_cores The number of cores to be used for parallel processing. The default is the half of the total number of cores available in the system.
#'
#' @return A vector of either the row indices or the actual row values based on the 'value' option
#'
#' @importFrom stringr str_split
#'
#' @examples
#' tokens5_plus <- bitToken_min_m(chatGPT_news1, "title", min_tokens = 5, value = TRUE, use_p = FALSE)
#'
#' @export
bitToken_min_m <- function(data, col_name, min_tokens = 3, value = FALSE, use_p = TRUE, num_cores = parallel::detectCores()) {
  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }

  # Check that col_name is a valid column name in the data frame
  if (!col_name %in% names(data)) {
    stop(paste("Error: '", col_name, "' is not a valid column name in the data frame.", sep = ""))
  }

  # Check that col_name contains text data
  if (!is.character(data[[col_name]]) && !is.factor(data[[col_name]])) {
    stop(paste("Error: '", col_name, "' does not contain text data. Please provide a column that contains character or factor data.", sep = ""))
  }


  # Limit the number of cores to a maximum of 8
  num_cores <- min(num_cores, parallel::detectCores() / 2)

  # Function to tokenize text
  tokenize_text <- function(text) stringr::str_split(text, "\\s+")

  if (use_p) {
    # Split the text column across multiple cores
    text_splits <- split(data[[col_name]], rep(1:num_cores, each = ceiling(nrow(data) / num_cores))[1:nrow(data)])

    # Apply stringr::str_split to each split in parallel
    tokens <- parallel::mclapply(text_splits, tokenize_text, mc.cores = num_cores)

    # Combine the token lists from each core
    tokens <- unlist(tokens, recursive = FALSE)
  } else {
    # Tokenize text without parallel processing
    tokens <- lapply(data[[col_name]], tokenize_text)
  }

  # Calculate the number of tokens and find rows with at least min_tokens
  num_tokens <- lengths(tokens)
  rows_with_min_tokens <- which(num_tokens >= min_tokens)

  # Return the row indices or values based on the value argument
  if (value) {
    return(data[rows_with_min_tokens, col_name])
  } else {
    return(rows_with_min_tokens)
  }
}
