#' Visualize Token Information for Character Vectors
#'
#' This function takes in a character vector or a list of character vectors and generates
#' a plot of their token information. The default plot type is a boxplot, but a histogram
#' plot is also available.
#'
#' @import graphics
#'
#' @param x a character vector or a list of character vectors.
#' @param type the type of plot to generate. Possible values are "boxplot" (default) and "histogram".
#' @param round an optional logical value indicating whether to round the output to integers.
#' The default value is FALSE.
#' @param remove_outliers an optional logical value indicating whether to remove outliers from the boxplot.
#' The default value is FALSE.
#' @param info an optional logical value indicating whether to print the median and mean values on the plot.
#' The default value is FALSE.
#'
#' @return A plot of the token information for the input character vectors.
#'
#' @examples
#' bitToken_viz(chatGPT_news1$title, type = "boxplot", remove_outliers = TRUE, info = TRUE)
#' bitToken_viz(chatGPT_news1$title, type = "histogram", info = TRUE)
#'
#' @export
bitToken_viz <- function(x, type = "boxplot", round = FALSE, remove_outliers = FALSE, info = FALSE) {
  # check if input is valid
  if (!is.character(x) && !is.list(x)) {
    stop("Invalid input. The input must be a character vector or a list of character vectors.")
  }

  # calculate token information
  token_info <- bitToken_info(x)

  if(type == "boxplot") {
    # generate boxplot
    if(remove_outliers) {
      boxplot(sapply(x, function(text) length(strsplit(text, "\\s+")[[1]])), outline=FALSE, ylab="Number of Tokens", ylim=c(0, max(token_info) + 1))
    } else {
      boxplot(sapply(x, function(text) length(strsplit(text, "\\s+")[[1]])), ylab="Number of Tokens", ylim=c(0, max(token_info) + 1))
    }
    # add labels to the boxplot
    labels <- paste0("Doc ", seq_along(x))
    axis(side=1, at=1:length(x), labels=labels, tick=FALSE)
    if(info) {
      # add a horizontal line for the median
      abline(h=token_info["median"], col="red", lty=2, lwd=2)
      # add a horizontal line for the mean
      abline(h=token_info["mean"], col="blue", lty=2, lwd=2)
    }
  } else if(type == "histogram") {
    # generate histogram
    hist(sapply(x, function(text) length(strsplit(text, "\\s+")[[1]])), breaks=seq(0, max(token_info) + 1, by=1), xlab="Number of Tokens", ylab="Frequency", main="")
    if(info) {
      # add a vertical line for the median
      abline(h=token_info["median"], col="red", lty=2, lwd=2)
      # add a horizontal line for the mean
      abline(h=token_info["mean"], col="blue", lty=2, lwd=2)
    }
  } else {
    stop("Invalid plot type. Possible values are 'boxplot' and 'histogram'.")
  }
}
