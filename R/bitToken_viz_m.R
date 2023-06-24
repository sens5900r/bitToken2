#' Visualize Token Information for Character Vectors (Multicore)
#'
#' This function takes in a character vector or a list of character vectors and generates
#' a plot of their token information. The default plot type is a boxplot, but a histogram
#' plot is also available.
#'
#' @param data A data frame.
#' @param text_column The name of the column that contains the text data.
#' @param type The type of plot to generate. Possible values are "boxplot" (default) and "histogram".
#' @param round An optional logical value indicating whether to round the output to integers.
#' The default value is FALSE.
#' @param rm_outliers An optional logical value indicating whether to remove outliers from the boxplot.
#' The default value is FALSE.
#' @param info An optional logical value indicating whether to print the median and mean values on the plot.
#' The default value is FALSE.
#' @param plot_title An optional character string for the plot title.
#' The default value is "Token Information Visualization".
#' @param use_p A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.
#' @param num_cores The number of cores to use for parallel processing.
#' By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
#' However, the number of cores used is limited to half of the total available cores.
#'
#' @return A plot of the token information for the input character vectors.
#'
#' @examples
#' bitToken_viz_m(chatGPT_news1, "title", rm_outliers = TRUE, info = TRUE, use_p = FALSE)
#' bitToken_viz_m(chatGPT_news1, "title", type = "histogram", info = TRUE, use_p = FALSE)
#'
#' @export
#' @import graphics parallel
bitToken_viz_m <- function(data, text_column, type = "boxplot", round = FALSE, rm_outliers = FALSE, info = FALSE, use_p = TRUE, plot_title = "Token Information Visualization", num_cores = parallel::detectCores()) {
  # check if input is valid
  if (!is.character(data[[text_column]])) {
    stop("Invalid input. The column must contain character data.")
  }

  # Limit the number of cores to a half of the total cores
  num_cores <- min(num_cores, parallel::detectCores() / 2)

  x <- data[[text_column]]

  # calculate token lengths using bitToken_m() or bitToken() depending on use_p
  token_lengths <- if (use_p) {
    bitToken_m(data, text_column, lengths = TRUE, use_p = use_p, num_cores = num_cores)
  } else {
    bitToken(data, text_column, lengths = TRUE)
  }

  # calculate token_info using bitToken_info_m() or bitToken_info() depending on use_p
  token_info <- if (use_p) {
    bitToken_info_m(data, text_column, use_p = use_p, num_cores = num_cores)
  } else {
    bitToken_info(data, text_column)
  }

  if(type == "boxplot") {
    # generate boxplot
    if(rm_outliers) {
      boxplot(token_lengths, outline=FALSE, ylab="Number of Tokens", ylim=c(0, max(token_lengths) + 1), main=plot_title)
    } else {
      boxplot(token_lengths, ylab="Number of Tokens", ylim=c(0, max(token_lengths) + 1), main=plot_title)
    }
    # add labels to the boxplot
    labels <- paste0("Doc ", seq_along(x))
    axis(side=1, at=1:length(x), labels=labels, tick=FALSE)
    if(info) {
      # add a horizontal line for the median
      abline(h=token_info["Median"], col="red", lty=2, lwd=2)
      # add a horizontal line for the mean
      abline(h=mean(token_lengths), col="blue", lty=2, lwd=2)
      legend("topright", c("Median", "Mean"), lty=c(1,1), col=c("red", "blue"), bty="n")
    }
  } else if(type == "histogram") {
    # generate histogram
    hist(token_lengths, breaks=seq(0, max(token_lengths) + 1, by=1), xlab="Number of Tokens", ylab="Frequency", main=plot_title)
    if(info) {
      # add a vertical line for the median
      abline(v=token_info["Median"], col="red", lty=2, lwd=2)
      # add a vertical line for the mean
      abline(v=mean(token_lengths), col="blue", lty=2, lwd=2)
      legend("topright", c("Median", "Mean"), lty=c(1,1), col=c("red", "blue"), bty="n")
    }
  } else {
    stop("Invalid plot type. Possible values are 'boxplot' and 'histogram'.")
  }
}
