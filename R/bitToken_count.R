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
#' @param location A boolean flag indicating whether to return the positions of tokens containing the specific pattern. Default is FALSE.
#' @param sum_info A boolean flag indicating whether to return summary information and frequency table for counts. Default is FALSE.
#'
#' @return A list or a numeric vector with the count of occurrences of the specific character or string for each row, and additional information based on the user options.
#'
#' @examples
#' data(chatGPT_news1, package="bitToken2")
#' chatGPT_news1$comma_count <- bitToken_count(chatGPT_news1, "title", pattern = ",")
#'
#' @export
#' @import stringr
bitToken_count <- function(data, text_column, pattern, location = NULL, sum_info = FALSE) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  # extract the data frame name
  data_name <- deparse(substitute(data))

  # count occurrences of the specific character or string in each row
  counts <- stringr::str_count(data[[text_column]], pattern)

  tokens <- stringr::str_split(data[[text_column]], "\\s+")

  if (is.null(location)) {
    positions <- lapply(tokens, function(token_list) {
      return(any(grepl(pattern, token_list, fixed = TRUE)))
    })
  } else {
    positions <- lapply(tokens, function(token_list) {
      if (length(token_list) >= location) {
        return(grepl(pattern, token_list[location], fixed = TRUE))
      } else {
        return(FALSE)
      }
    })
  }

  if (sum_info) {
    summary_info <- summary(counts)
    freq_table <- sort(table(counts), decreasing = TRUE)
    title <- paste("Summary for pattern '", pattern, "' in column '", text_column, "'", "in dataset, ", data_name, sep = "")
  }

  # return output based on user options
  if (sum_info) {
    return(list(title = title, counts = counts, positions = positions, summary = summary_info, frequency_table = freq_table))
  } else {
    return(list(counts = counts, positions = positions))
  }
}
