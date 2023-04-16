## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=F------------------------------------------------------------------
#  devtools::install_github("sens5900r/bitToken2")

## ---- eval=F------------------------------------------------------------------
#  library(bitToken2)

## ---- eval=F------------------------------------------------------------------
#  comma_counts <- bitToken_count(chatGPT_news1, "title", pattern = ",")

## ---- eval=F------------------------------------------------------------------
#  ordered_tokens <- bitToken_order(chatGPT_news1, "title", 1)

## ---- eval=F------------------------------------------------------------------
#  bitToken_viz(chatGPT_news1, "title", type = "boxplot", rm_outliers = TRUE, info = TRUE)

## ---- eval=F------------------------------------------------------------------
#  bitToken_viz(chatGPT_news1, "title", type = "histogram", info = TRUE)

## ---- eval=F------------------------------------------------------------------
#  rows_with_min_tokens <- bitToken_min(chatGPT_news1, "title", min_tokens = 5, value = FALSE)

## ---- eval=F------------------------------------------------------------------
#  rows_with_min_tokens_values <- bitToken_min(chatGPT_news1, "title", min_tokens = 5, value = TRUE)

## ---- eval=F------------------------------------------------------------------
#  
#  # Tokenize text data in the "title" column using multi-core support
#  tokens <- bitToken_m(data = chatGPT_news1, text_column = "title")
#  
#  # Extract Token in Descending Order by Frequency with Multi-Core Support
#  bitToken_info_m(chatGPT_news1, "title", add=TRUE)

