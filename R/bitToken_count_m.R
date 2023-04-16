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
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#' @param location A boolean flag indicating whether to return the positions of tokens containing the specific pattern. Default is FALSE.
#' @param sum_info A boolean flag indicating whether to return summary information and frequency table for counts. Default is FALSE.
#'
#' @return A list or a numeric vector with the count of occurrences of the specific character or string for each row, and additional information based on the user options.
#'
#' @examples
#' apple_count <- bitToken_count_m(chatGPT_news1, "title", pattern = "(?i)apple", use_p = FALSE)
#'
#' @export
#' @import stringr
bitToken_count_m <- function(data, text_column, pattern, location = FALSE, sum_info = FALSE, use_p = TRUE) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  # extract the data frame name
  data_name <- deparse(substitute(data))

  # Get the number of available cores
  num_cores <- parallel::detectCores()

  # Limit the number of cores to a maximum of 8
  num_cores <- min(num_cores, 8)

  # count occurrences of the specific character or string in each row
  if (use_p) {
    counts <- parallel::mclapply(data[[text_column]], function(text) stringr::str_count(text, pattern), mc.cores = num_cores)
  } else {
    counts <- lapply(data[[text_column]], function(text) stringr::str_count(text, pattern))
  }

  if (location) {
    tokens <- if (use_p) {
      parallel::mclapply(data[[text_column]], function(text) stringr::str_split(text, "\\s+"), mc.cores = num_cores)
    } else {
      lapply(data[[text_column]], function(text) stringr::str_split(text, "\\s+"))
    }
    positions <- if (use_p) {
      parallel::mclapply(tokens, function(token_list) {
        match_positions <- which(grepl(pattern, token_list, fixed = TRUE))
        if (length(match_positions) == 0) {
          return(0)
        } else {
          return(match_positions)
        }
      }, mc.cores = num_cores)
    } else {
      lapply(tokens, function(token_list) {
        match_positions <- which(grepl(pattern, token_list, fixed = TRUE))
        if (length(match_positions) == 0) {
          return(0)
        } else {
          return(match_positions)
        }
      })
    }
  }

  if (sum_info) {
    summary_info <- summary(counts)
    freq_table <- sort(table(counts), decreasing = TRUE)
    title <- paste("Summary for pattern '", pattern, "' in column '", text_column, "'", "in dataset, ", data_name, sep = "")
  }

  # return output based on user options
  if (location && sum_info) {
    return(list(title = title, counts = counts, positions = positions, summary = summary_info, frequency_table = freq_table))
  } else if (location) {
    return(list(counts = counts, positions = positions))
  } else if (sum_info) {
    return(list(title = title, counts = counts, summary = summary_info, frequency_table = freq_table))
  } else {
    return(counts)
  }
}
