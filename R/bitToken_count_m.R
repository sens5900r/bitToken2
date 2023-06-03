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
#' @import stringr parallel
bitToken_count_m <- function(data, text_column, pattern, location = NULL, sum_info = FALSE, num_cores = parallel::detectCores()) {
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  data_name <- deparse(substitute(data))

  # Limit the number of cores to a half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)

  if (is.null(location)) {
    counts <- parallel::mclapply(data[[text_column]], stringr::str_count, pattern = pattern, mc.cores = num_cores)
  } else {
    tokens <- parallel::mclapply(data[[text_column]], stringr::str_split, pattern = "\\s+", mc.cores = num_cores)

    counts <- parallel::mclapply(tokens, function(token_list) {
      if (length(unlist(token_list)) >= location) {
        sum(grepl(pattern, unlist(token_list)[location], fixed = TRUE))
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
