## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=F------------------------------------------------------------------
#  install.packages("bitToken2")

## ---- eval=F------------------------------------------------------------------
#  library(bitToken2)

## ---- eval=F------------------------------------------------------------------
#  comma_counts <- bitToken_count(data = chatGPT_news1, text_column = "title", pattern = ",")

## ---- eval=F------------------------------------------------------------------
#  ordered_tokens <- bitToken_order(chatGPT_news1, "title", 1)

## ---- eval=F------------------------------------------------------------------
#  bitToken_viz(data = chatGPT_news1, text_column = "title", type = "boxplot", remove_outliers = TRUE, info = TRUE)

## ---- eval=F------------------------------------------------------------------
#  bitToken_viz(data = chatGPT_news1, text_column = "title", type = "histogram", info = TRUE)

## ---- eval=F------------------------------------------------------------------
#  rows_with_min_tokens <- bitToken_min(data = chatGPT_news1, col_name = "title", min_tokens = 5, value = FALSE)

## ---- eval=F------------------------------------------------------------------
#  rows_with_min_tokens_values <- bitToken_min(data = chatGPT_news1, col_name = "title", min_tokens = 5, value = TRUE)

