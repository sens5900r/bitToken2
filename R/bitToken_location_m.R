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
#' @import stringr
#' @export
bitToken_location_m <- function(data, text_column, pattern, use_p = TRUE, num_cores = parallel::detectCores()) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }

  # Limit the number of cores to a half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)

  # Tokenize the text column by splitting on whitespace
  tokens <- stringr::str_split(data[[text_column]], "\\s+")

  # Construct a vector marking the locations of the specified pattern
  positions <- parallel::mclapply(tokens, function(token_list) {
    unname(sapply(token_list, function(token) {
      as.integer(grepl(pattern, token, fixed = TRUE))
    }))
  }, mc.cores = num_cores)

  return(positions)
}
