#' Token Count (Multicore)
#'
#' Count the occurrences of a specific character or string in each row of the specified text column in a data frame.
#'
#' @param data A data frame containing the text data.
#' @param text_column A string specifying the name of the text column in the input data.
#' @param pattern A character string representing the specific character or string to filter.
#'   Note the following when using the pattern argument:
#'   - Case sensitivity: By default, the function is case sensitive. Use (?i) within the pattern to make it case insensitive.
#'   - Regular expressions: The function uses regular expressions, so escape special characters (e.g., . or *) with a backslash (\\).
#'   - Spaces and whitespace: Include the exact whitespace character in the pattern, such as regular spaces or tabs, or use \\s to represent any whitespace.
#' @param location A numeric value indicating the position of tokens to consider when counting. If specified,
#' the function checks whether the pattern appears at the given token position in each row. Default is NULL,
#' indicating all tokens are considered.
#' @param sum_info A boolean flag indicating whether to return summary information and frequency table for counts. Default is FALSE.
#' @param num_cores The number of cores to be used for parallel processing. The default is the half of the total number of cores available in the system.
#'
#' @return A character vector with the filtered values from the specified text column.
#'
#' @examples
#' data(chatGPT_news1, package="bitToken2")
#' chatGPT_news1$comma_count <- bitToken_count_m(chatGPT_news1, "title", pattern = ",", num_cores = 2)
#'
#' @export
#' @import parallel
#' @import data.table
#' @import stringi
bitToken_count_m <- function(data, text_column, pattern, location = NULL, sum_info = FALSE, num_cores = parallel::detectCores()) {
  # Checking for correct input type and column existence
  if (!is.data.frame(data)) {
    stop("Invalid input: 'data' must be a data frame. Also, ensure you have not quoted the data frame name.")
  }
  
  # Convert to data.table
  data <- data.table::as.data.table(data)
  
  # Check if column exists in data.table
  if (!(text_column %in% names(data))) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  # Checking for correct input in num_cores
  if (!is.numeric(num_cores) || length(num_cores) != 1 || num_cores <= 0 || num_cores %% 1 != 0) {
    stop("Invalid input: 'num_cores' should be a positive integer.")
  }

  # Ensuring num_cores does not exceed half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)  
  if (is.null(location)) {
    counts <- parallel::mclapply(data[[text_column]], stringi::stri_count_fixed, pattern = pattern, mc.cores = num_cores)
  } else {
    tokens <- parallel::mclapply(data[[text_column]], stringi::stri_split_fixed, pattern = " ", mc.cores = num_cores)
    
    counts <- parallel::mclapply(tokens, function(token_list) {
      if (length(unlist(token_list)) >= location) {
        sum(stringi::stri_detect_fixed(unlist(token_list)[location], pattern))
      } else {
        0
      }
    }, mc.cores = num_cores)
  }
  
  # Unlist the results
  counts <- unlist(counts)
  
  if (sum_info) {
    summary_info <- summary(counts)
    freq_table <- sort(table(counts), decreasing = TRUE)
    title <- paste("Summary for pattern '", pattern, "' in column '", text_column, "'", "in dataset, ", data_name, sep = "")
    return(list(title = title, counts = counts, summary = summary_info, frequency_table = freq_table))
  } else {
    return(counts)
  }
}