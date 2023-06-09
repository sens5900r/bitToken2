#' Token Search (Multicore)
#'
#' Filter rows based on the occurrence of a specific character or string at a specific location in the specified text column in a data frame.
#' This function leverages multiple cores for parallel execution.
#'
#' @param data A data frame containing the text data.
#' @param text_column A string specifying the name of the text column in the input data.
#' @param pattern A character string representing the specific character or string to filter.
#'   Note the following when using the pattern argument:
#'   - Case sensitivity: By default, the function is case sensitive. Use (?i) within the pattern to make it case insensitive.
#'   - Regular expressions: The function uses regular expressions, so escape special characters (e.g., . or *) with a backslash (\\).
#'   - Spaces and whitespace: Include the exact whitespace character in the pattern, such as regular spaces or tabs, or use \\s to represent any whitespace.
#' @param another_column A string specifying the name of the column in the input data from which to return the output.
#' If not specified, the function will return the output from the specified text_column.
#' @param location A numeric value indicating the position of tokens to consider when filtering. Default is NULL, indicating all tokens are considered.
#' @param num_cores The number of cores to use for parallel processing.
#' By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
#' However, the number of cores used is limited to half of the total available cores.
#' @return A character vector with the filtered values from the specified text column.
#' @examples
#' # Search for 'COVID' at second position in titles using multiple cores
#' titles_COVID <- bitToken_search_m(chatGPT_news1, "title", "COVID", location=2, num_cores=2)
#' @export
#' @import data.table
#' @import stringi
#' @import parallel
bitToken_search_m <- function(data, text_column, pattern, another_column = NULL, location = NULL, num_cores = parallel::detectCores()) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  data <- data.table::as.data.table(data)
  
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  if (!is.null(another_column) && !another_column %in% names(data)) {
    stop(paste("Error: '", another_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  # Limit the number of cores to a half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)
  
  if (is.null(location)) {
    positions <- stringi::stri_detect_fixed(data[[text_column]], pattern)
  } else {
    tokens <- stringi::stri_split_fixed(data[[text_column]], " ")
    positions <- unlist(parallel::mclapply(tokens, function(token_list) {
      if (length(token_list) >= location) {
        return(stringi::stri_detect_fixed(token_list[location], pattern))
      } else {
        return(FALSE)
      }
    }, mc.cores = num_cores))
  }
  # if another_column is not specified, use text_column
  if (is.null(another_column)) {
    another_column <- text_column
  }
  
  # Extract the positions where the condition is true
  filtered_data <- data[positions, another_column, with = FALSE]
  
  return(filtered_data)
}
