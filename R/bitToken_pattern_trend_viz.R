#' Plot Count and Moving Average Count Over Time
#'
#' This function takes a data frame and the names of two count variables, plots these variables over time, and returns the resulting ggplot object.
#'
#' @param df A data frame.
#' @param x_var A character string specifying the name of the x variable (should be a Date column).
#' @param y_var1 A character string specifying the name of the first y variable (Count).
#' @param y_var2 A character string specifying the name of the second y variable (Moving Average Count).
#' @param window A numeric value specifying the window size for calculating moving averages. If NULL or greater than the number of observations in df, no moving averages are calculated.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' bitToken_pattern_trend_viz(df = df, x_var = "date", y_var1 = "count", y_var2 = "count_ma", window = 7)
#' }
#' @export
bitToken_pattern_trend_viz <- function(df, x_var, y_var1, y_var2, window = NULL){
  
  # Order data frame by date
  df <- df[order(df[[x_var]]),]
  
  # Check if window size is appropriate for calculating moving averages
  if (!is.null(window)) {
    if (window > nrow(df)) {
      warning("The window size for calculating moving averages is larger than the number of observations. Moving averages will not be calculated.")
    } else {
      # Calculate moving averages
      df[, ':=' (count_ma = zoo::rollmean(get(y_var1), window, fill = NA, align = "right"),
                 count_ma = zoo::rollmean(get(y_var2), window, fill = NA, align = "right"))]
    }
  }
  
  # Plot the data
  gg <- ggplot(df, aes_string(x = x_var)) +
    geom_line(aes_string(y = y_var1, colour = "'Count'"), alpha = 0.5, size = 0.2) +
    geom_line(aes_string(y = y_var2, colour = "'Count MA'"), , size = 1.2) +
    scale_x_date(date_labels = "%m-%d", date_breaks = "7 days") +
    ylab("Count") +
    xlab(" ") +
    ggtitle(paste0("`", y_var1, "` over time")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 1, hjust = 1)) +
    theme(legend.position = "none",
          panel.grid.major = element_line(color = "grey90", size = 0.25),
          panel.grid.minor = element_line(color = "grey90", size = 0.1)) +
    scale_color_manual(values = c("Count" = "blue", "Count MA" = "red"))
  
  return(gg)
}
