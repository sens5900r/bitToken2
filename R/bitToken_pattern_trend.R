#' Compute text pattern trend over time
#'
#' This function takes a dataframe, a text column name, a pattern to search for, and optionally a date column name and a moving average window. It computes the count, percentage, occurrence, occurrence percentage, and occurrence per count of the pattern in the text column on a daily basis if a date column is given, and then computes their moving averages if a window is given. It finally plots these trend lines over time.
#'
#' @param data a data frame or a data.table
#' @param text_column a character string specifying the name of the text column in the data frame
#' @param pattern a character string specifying the pattern to search for in the text column
#' @param date_column a character string specifying the name of the date column in the data frame (optional)
#' @param window an integer specifying the moving average window (optional)
#'
#' @return A data.table with columns date, total, count, percentage, occurrence, occurrence percentage, and occurrence per count, as well as their moving averages if a window is given. If a date column is not given, a data.table with one row and the same columns (except date) is returned.
#'
#' @examples
#' \dontrun{
#' bitToken_pattern_trend(data = data, text_column = "text", pattern = "bitcoin", date_column = "date", window = 7)
#' }
#' @export
bitToken_pattern_trend <- function(data, text_column, pattern, date_column = NULL, window = NULL) {
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Invalid input: 'data' must be a data frame or a data.table.")
  }
  
  if (!text_column %in% names(data)) {
    stop(paste("Invalid input: '", text_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  if (!is.character(data[[text_column]])) {
    stop(paste("Invalid input: '", text_column, "' must be a character column.", sep = ""))
  }
  
  if (is.data.frame(data)) {
    data <- data.table::as.data.table(data)
  }
  
  if (!is.null(date_column) && !date_column %in% names(data)) {
    stop(paste("Invalid input: '", date_column, "' is not a valid column name in the data frame.", sep = ""))
  }
  
  if (!is.null(date_column)) {
    result <- data[, .(day = weekdays(as.Date(get(date_column)), abbreviate = TRUE),
                       total = .N,
                       count = sum(stri_detect_fixed(get(text_column), pattern)),
                       perc = sum(stri_detect_fixed(get(text_column), pattern)) / .N * 100, 
                       occ = sum(stri_count_fixed(get(text_column), pattern)),
                       occ_perc = sum(stri_count_fixed(get(text_column), pattern)) / .N * 100,
                       occ_per_count = ifelse(sum(stri_detect_fixed(get(text_column), pattern)) == 0, 0, sum(stri_count_fixed(get(text_column), pattern)) / sum(stri_detect_fixed(get(text_column), pattern)))
    ), by = .(date = as.Date(get(date_column)))]
    
    setorder(result, -date) # Sorting in descending order
    result <- result[-1, ] # Exclude the first row
    
    # Compute moving averages if window parameter is given
    if (!is.null(window)) {
      # Order the data by date (from oldest to newest)
      result <- result[order(result[[date_column]])]
      
      # Calculate moving averages
      result[, ':=' (count_ma = zoo::rollmean(count, window, fill = NA, align = "right"),
                     perc_ma = zoo::rollmean(perc, window, fill = NA, align = "right"),
                     occ_ma = zoo::rollmean(occ, window, fill = NA, align = "right"),
                     occ_perc_ma = zoo::rollmean(occ_perc, window, fill = NA, align = "right"),
                     occ_per_count_ma = zoo::rollmean(occ_per_count, window, fill = NA, align = "right"))]
      
      # Order the data by date (from newest to oldest)
      result <- result[order(-result[[date_column]])]
    }
  }
  
  else {
    pattern_rows <- stringi::stri_detect_fixed(data[[text_column]], pattern)
    total <- nrow(data)
    count <- sum(pattern_rows)
    perc <- count / total * 100
    occ <- sum(stringi::stri_count_fixed(data[[text_column]], pattern))
    occ_perc <- sum(stringi::stri_count_fixed(data[[text_column]], pattern)) / total * 100
    occ_per_count <- if(count == 0) {
      0 
    } else {
      (occ / count)
    }
    
    result <- data.table::data.table(total = total, count = count, 
                                     perc = perc, occ = occ,
                                     occ_perc = occ_perc, 
                                     occ_per_count = occ_per_count)
    
    # Calculate moving averages if window parameter is given
    if (!is.null(window)) {
      result[, `:=` (count_ma = zoo::rollmean(count, window, fill = NA, align = "right"),
                     perc_ma = zoo::rollmean(perc, window, fill = NA, align = "right"),
                     occ_ma = zoo::rollmean(occ, window, fill = NA, align = "right"),
                     occ_perc_ma = zoo::rollmean(occ_perc, window, fill = NA, align = "right"),
                     occ_per_count_ma = zoo::rollmean(occ_per_count, window, fill = NA, align = "right"))]
    }
  }
  
  return(result)
}
