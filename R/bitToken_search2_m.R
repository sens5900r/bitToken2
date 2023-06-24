#' Bit Token Search 2 Multicore
#'
#' This function searches for a specific word in a paragraph style text and returns the sentences containing the word. 
#' The function can optionally return the index of the original rows. This function leverages multiple cores for parallel execution.
#'
#' @param data A data frame containing the text data.
#' @param text_column A string specifying the name of the text column in the input data.
#' @param pattern A character string representing the specific character or string to filter.
#' @param index A logical value indicating whether to return the index of the original rows. 
#'   Default is FALSE. If TRUE, the function returns a list of lists, 
#'   each containing the index of the original row and the filtered sentences from that row.
#' @param num_cores The number of cores to use for parallel processing.
#' By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
#' However, the number of cores used is limited to half of the total available cores.
#' @return If index = FALSE, a character vector with the filtered sentences.
#'   If index = TRUE, a list of lists, each containing the index of the original row and the filtered sentences from that row.
#' @examples
#' # Search for 'COVID' in text column without index using multiple cores
#' sentences_COVID <- bitToken_search2_m(chatGPT_news1, "title", "COVID", num_cores = 2)
#' # Search for 'COVID' in text column with index using multiple cores
#' sentences_COVID_index <- bitToken_search2_m(chatGPT_news1, "title", "COVID", index = TRUE, num_cores = 2)
#' @export
#' @import data.table
#' @import stringi
#' @import parallel
bitToken_search2_m <- function(data, text_column, pattern, index = FALSE, num_cores = parallel::detectCores()) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  # Limit the number of cores to a half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)
  
  # convert to data.table
  data <- data.table::as.data.table(data)
  
  # Split the text_column into sentences using '. ' as delimiter
  sentences <- stringi::stri_split_fixed(data[[text_column]], ".")
  
  # Filter the sentences based on the pattern using parallel processing
  filtered_sentences <- parallel::mclapply(seq_along(sentences), function(i) {
    sentence_list <- sentences[[i]]
    matches <- stringi::stri_detect_fixed(sentence_list, pattern)
    matched_sentences <- sentence_list[matches]
    
    if (length(matched_sentences) > 0) {
      return(list(index = i, sentences = matched_sentences))
    } else {
      return(NULL)
    }
  }, mc.cores = num_cores)
  
  # Remove NULL elements
  filtered_sentences <- Filter(Negate(is.null), filtered_sentences)
  
  # If index is FALSE, only return the sentences
  if (!index) {
    filtered_sentences <- lapply(filtered_sentences, `[[`, "sentences")
    # Flatten the list to a vector
    filtered_sentences <- unlist(filtered_sentences)
  }
  
  return(filtered_sentences)
}