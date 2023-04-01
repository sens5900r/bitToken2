#' Token Information
#'
#' Get information about the number of tokens in a character vector or list of character vectors.
#'
#' @param x A character vector or a list of character vectors.
#' @param add A logical indicating whether to include additional information about the data.
#' @param min_t The minimum number of tokens to consider for summary statistics.
#' @param max_t The maximum number of tokens to consider for summary statistics.
#'
#' @return A named numeric vector with the minimum, median, mode, and maximum number of tokens.
#'
#' @examples
#' bitToken_info(chatGPT_news1$title)
#' bitToken_info(chatGPT_news1$title, min_t = 5, max_t = 20, add = TRUE)
#'
#' @export
bitToken_info <- function(x, add = FALSE, min_t = 1, max_t = Inf) {
  # check if input is valid
  if (!is.character(x) && !is.list(x)) {
    stop("Invalid input. The input must be a character vector or a list of character vectors.")
  }

  # tokenize text data
  tokens <- lapply(x, str_split, pattern = "\\s+")

  # calculate token information
  token_count <- sapply(tokens, lengths)
  valid_tokens <- token_count[token_count >= min_t & token_count <= max_t]
  token_info <- c(
    Min. = min(valid_tokens),
    Median = median(valid_tokens),
    Mode = as.integer(names(table(valid_tokens)[which.max(table(valid_tokens))])),
    Max. = max(valid_tokens)
  )

  names(token_info) <- c("Min.", "Median", "Mode", "Max.")

  # additional information if add is TRUE
  if (add) {
    num_rows <- length(x)
    mean_tokens <- round(mean(token_count), 2)
    prop_valid <- round(sum(token_count >= min_t & token_count <= max_t) / num_rows, 2)
    cat("Number of rows:", num_rows, "\n")
    cat("Mean number of tokens:", mean_tokens, "\n")
    cat(paste0("Proportion of rows with ", min_t, " to ", max_t, " tokens:", prop_valid, "\n"))
  }

  # return output
  return(token_info)
}
