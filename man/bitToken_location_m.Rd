% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitToken_location_m.R
\name{bitToken_location_m}
\alias{bitToken_location_m}
\title{Locate specific pattern in a text column of a data frame (Multicore)}
\usage{
bitToken_location_m(
  data,
  text_column,
  pattern,
  use_p = TRUE,
  num_cores = parallel::detectCores()
)
}
\arguments{
\item{data}{Data frame that contains the text column.}

\item{text_column}{Name of the column in the data frame that contains the text to search for the pattern.}

\item{pattern}{Pattern to locate in the text.}

\item{use_p}{A logical value. If \code{FALSE}, the function will not use the 'use_p' argument in its calculations. Default is \code{TRUE}.}

\item{num_cores}{The number of cores to use for parallel processing.
By default, it is set to the number of available cores detected by \code{parallel::detectCores()}.
However, the number of cores used is limited to half of the total available cores.}
}
\value{
A list of vectors indicating the presence of the pattern in each token. The length of the vector is equal to the number of tokens in the corresponding row.
        Each element in the vector is either 0 (the pattern is not present) or 1 (the pattern is present).
}
\description{
This function locates a specific pattern in a text column of a data frame.
It returns a binary vector indicating the presence of the pattern in each token.
This function leverages multiple cores for parallel execution.
}
\examples{
bitToken_location_m(chatGPT_news1, "title", "COVID", num_cores = 2)
}
