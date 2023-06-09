% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitToken_search2_m.R
\name{bitToken_search2_m}
\alias{bitToken_search2_m}
\title{Bit Token Search 2 Multicore}
\usage{
bitToken_search2_m(
  data,
  text_column,
  pattern,
  index = FALSE,
  num_cores = parallel::detectCores()
)
}
\arguments{
\item{data}{A data frame containing the text data.}

\item{text_column}{A string specifying the name of the text column in the input data.}

\item{pattern}{A character string representing the specific character or string to filter.}

\item{index}{A logical value indicating whether to return the index of the original rows. 
Default is FALSE. If TRUE, the function returns a list of lists, 
each containing the index of the original row and the filtered sentences from that row.}

\item{num_cores}{The number of cores to use for parallel processing.
By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
However, the number of cores used is limited to half of the total available cores.}
}
\value{
If index = FALSE, a character vector with the filtered sentences.
  If index = TRUE, a list of lists, each containing the index of the original row and the filtered sentences from that row.
}
\description{
This function searches for a specific word in a paragraph style text and returns the sentences containing the word. 
The function can optionally return the index of the original rows. This function leverages multiple cores for parallel execution.
}
\examples{
# Search for 'COVID' in text column without index using multiple cores
sentences_COVID <- bitToken_search2_m(chatGPT_news1, "title", "COVID", num_cores = 2)
# Search for 'COVID' in text column with index using multiple cores
sentences_COVID_index <- bitToken_search2_m(chatGPT_news1, "title", "COVID", index = TRUE, num_cores = 2)
}
