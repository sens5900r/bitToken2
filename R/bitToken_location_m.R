#' Locate specific pattern in a text column of a data frame (Multicore)
#'
#' This function locates a specific pattern in a text column of a data frame.
#' It returns a binary vector indicating the presence of the pattern in each token.
#' This function leverages multiple cores for parallel execution.
#'
#' @param data Data frame that contains the text column.
#' @param text_column Name of the column in the data frame that contains the text to search for the pattern.
#' @param pattern Pattern to locate in the text.
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#' @param num_cores The number of cores to use for parallel processing.
#' By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
#' However, the number of cores used is limited to half of the total available cores.
#'
#' @return A list of vectors indicating the presence of the pattern in each token. The length of the vector is equal to the number of tokens in the corresponding row.
#'         Each element in the vector is either 0 (the pattern is not present) or 1 (the pattern is present).
#'
#' @examples
#' bitToken_location_m(chatGPT_news1, "title", "COVID", num_cores = 2)
#' @import parallel
#' @import stringi
#' @import data.table
#' @export
bitToken_location_m <- function(data, text_column, pattern, use_p = TRUE, num_cores = parallel::detectCores()) {
  # check if data is a data frame or a data.table
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
  # Limit the number of cores to a half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)
  
  # convert to data.table
  dt <- data.table::as.data.table(data)
  
  # Tokenize the text column by splitting on whitespace
  tokens <- stringi::stri_split_fixed(dt[[text_column]], " ")
  
  # Construct a vector marking the locations of the specified pattern
  positions <- parallel::mclapply(tokens, function(token_list) {
    unname(sapply(token_list, function(token) {
      as.integer(grepl(pattern, token, fixed = TRUE))
    }))
  }, mc.cores = num_cores)
  
  return(positions)
}
