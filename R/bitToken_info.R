#' Token Information
#'
#' Get information about the number of tokens in a character vector or list of character vectors.
#'
#' @param data A character vector or a list of character vectors.
#' @param text_column A string specifying the name of the text column in the input data.
#' @param add A logical indicating whether to include additional information about the data.
#' @param min_t The minimum number of tokens to consider for summary statistics.
#' @param max_t The maximum number of tokens to consider for summary statistics.
#'
#' @return A named numeric vector with the minimum, median, mode, and maximum number of tokens.
#'
#' @examples
#' data("chatGPT_news1", package = "bitToken2")
#' bitToken_info(chatGPT_news1, "title")
#' bitToken_info(chatGPT_news1, "title", min_t = 3, max_t = 10, add = TRUE)
#'
#' @export
#' @import data.table
#' @import stringi
bitToken_info <- function(data, text_column, add = FALSE, min_t = 1, max_t = Inf) {
  # check if data is a data.frame or a data.table
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Invalid input: 'data' must be a data frame or a data.table.")
  }
  
  # check if text_column is a valid column
  if (!text_column %in% names(data)) {
    stop(paste("Invalid input: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  # check if text_column contains character data
  if (!is.character(data[[text_column]])) {
    stop(paste("Invalid input: '", text_column, "' must be a character column.", sep = ""))
  }
  
  # check if min_t and max_t are valid
  if (!is.numeric(min_t) || !is.numeric(max_t) || length(min_t) != 1 || length(max_t) != 1) {
    stop("Invalid input: 'min_t' and 'max_t' must be single numeric values.")
  }
  
  # Convert data to data.table if it is a data.frame
  if (is.data.frame(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # tokenize text data
  tokens <- stringi::stri_split(data[[text_column]], regex = "\\s+")
  
  # calculate token information
  token_count <- vapply(tokens, length, integer(1L))
  valid_tokens <- token_count[token_count >= min_t & token_count <= max_t]
  
  token_info <- c(
    Min. = min(valid_tokens),
    Median = stats::median(valid_tokens),
    Mode = as.integer(names(which.max(table(valid_tokens)))),
    Max. = max(valid_tokens)
  )
  
  names(token_info) <- c("Min.", "Median", "Mode", "Max.")
  
  # additional information if add is TRUE
  if (add) {
    num_rows <- nrow(data)
    mean_tokens <- round(mean(token_count), 2)
    prop_valid <- round(sum(token_count >= min_t & token_count <= max_t) / num_rows, 2)
    object_size <- format(utils::object.size(data), units = "auto", standard = "SI")
    mean_size <- format(utils::object.size(data)/num_rows, units = "auto", standard = "SI")
    cat("Object size:", object_size, "\n")
    cat("Number of rows:", num_rows, "\n")
    cat(paste0("Mean size per row:", mean_size, "\n"))
    cat("Mean number of tokens:", mean_tokens, "\n")
    cat(paste0("Proportion of rows with ", min_t, " to ", max_t, " tokens:", prop_valid, "\n"))
    
    # Print token info summary for the specified range
    cat("\nToken info summary within the specified range:\n")
    print(token_info)
    
    # Add token count summary table within the specified range
    token_count_table <- table(token_count[token_count >= min_t & token_count <= max_t])
    cat("\nToken count summary within the specified range:\n")
    print(token_count_table)
  } else {
    # return output
    return(token_info)
  }
}
