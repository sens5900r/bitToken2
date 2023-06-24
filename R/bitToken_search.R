#' @import data.table
#' @import stringi
bitToken_search <- function(data, text_column, pattern, another_column = NULL, location = NULL) {
  # check if input is valid
  if (!is.data.frame(data)) {
    stop("Invalid input. The input must be a data frame.")
  }
  
  # convert to data.table
  data <- data.table::as.data.table(data)
  
  # check if column exists in data.table
  if (!text_column %in% names(data)) {
    stop(paste("Error: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  if (!is.null(another_column) && !another_column %in% names(data)) {
    stop(paste("Error: '", another_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  if (is.null(location)) {
    positions <- stringi::stri_detect_fixed(data[[text_column]], pattern)
  } else {
    tokens <- stringi::stri_split_fixed(data[[text_column]], " ")
    positions <- vapply(tokens, function(token_list) {
      if (length(token_list) >= location) {
        return(stringi::stri_detect_fixed(token_list[location], pattern))
      } else {
        return(FALSE)
      }
    }, logical(1))
  }
  
  # if another_column is not specified, use text_column
  if (is.null(another_column)) {
    another_column <- text_column
  }
  
  # Extract the positions where the condition is true
  filtered_data <- data[positions, another_column, with = FALSE]
  
  return(filtered_data)
}
