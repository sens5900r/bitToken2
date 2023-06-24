#' Token Count
#'
#' Count the occurrences of a specific character or string in each row of the specified text column in a data frame.
#'
#' @param data A data frame containing the text data.
#' @param text_column A string specifying the name of the text column in the input data.
#' @param pattern A character string representing the specific character or string to count.
#'   Note the following when using the pattern argument:
#'   - Case sensitivity: By default, the function is case sensitive. Use (?i) within the pattern to make it case insensitive.
#'   - Regular expressions: The function uses regular expressions, so escape special characters (e.g., . or *) with a backslash (\\).
#'   - Spaces and whitespace: Include the exact whitespace character in the pattern, such as regular spaces or tabs, or use \\s to represent any whitespace.
#' @param location A numeric value indicating the position of tokens to consider when counting. If specified,
#' the function checks whether the pattern appears at the given token position in each row. Default is NULL,
#' indicating all tokens are considered.
#' @param sum_info A boolean flag indicating whether to return summary information and frequency table for counts. Default is FALSE.
#'
#' @return A list or a numeric vector with the count of occurrences of the specific character or string for each row, and additional information based on the user options.
#'
#' @examples
#' data(chatGPT_news1, package="bitToken2")
#' chatGPT_news1$comma_count <- bitToken_count(chatGPT_news1, "title", pattern = ",")
#'
#' @export
#' @import data.table
#' @import stringi
bitToken_count <- function(data, text_column, pattern, location = NULL, sum_info = FALSE) {
  # Check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input: 'data' must be a data frame. Also, ensure you have not quoted the data frame name.")
  }
  
  if (!is.character(text_column)) {
    stop("Invalid input: 'text_column' must be a string.")
  }
  
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  if (!is.character(pattern)) {
    stop("Invalid input: 'pattern' must be a string.")
  }
  
  if (!is.null(location) && (!is.numeric(location) || length(location) != 1 || location <= 0)) {
    stop("Invalid input: 'location' must be a positive numeric value.")
  }
  
  if (!is.logical(sum_info) || length(sum_info) != 1) {
    stop("Invalid input: 'sum_info' must be a single logical value.")
  }
  
  # Convert to data.table
  data <- data.table::as.data.table(data)
  
  # check if column exists in data.table
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  data_name <- deparse(substitute(data))
  
  if (is.null(location)) {
    counts <- stringi::stri_count_fixed(data[[text_column]], pattern)
  } else {
    tokens <- stringi::stri_split_fixed(data[[text_column]], " ")
    counts <- vapply(tokens, function(token_list) {
      if (length(token_list) >= location) {
        sum(stringi::stri_detect_fixed(token_list[location], pattern))
      } else {
        0
      }
    }, numeric(1))
  }
  
  if (sum_info) {
    summary_info <- summary(counts)
    freq_table <- sort(table(counts), decreasing = TRUE)
    title <- paste("Summary for pattern '", pattern, "' in column '", text_column, "'", "in dataset, ", data_name, sep = "")
    return(list(title = title, counts = counts, summary = summary_info, frequency_table = freq_table))
  } else {
    return(counts)
  }
}
