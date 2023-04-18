# **bitToken2**

bitToken2 is an R package for tokenizing text data in a data frame. It provides a simple interface for splitting text data into tokens, which can be useful for natural language processing tasks like sentiment analysis, topic modeling, and text classification. The latest version of the package (0.4.1) includes multi-core support for some functions.

## **Installation**

You can install **`bitToken2`** from GitHub using the **`devtools`** package:

```{r}
devtools::install_github("sens5900r/bitToken2")
```

## **Usage**

Here's an example of how to use **`bitToken2`** to tokenize text data in a data frame:

```{r}
library(bitToken2)

# Load example data
data(chatGPT_news1)

# Tokenize text data in the "title" column in a dataframe
tokens <- bitToken(chatGPT_news1, "title")

# View the first 6 tokens
head(tokens)

# recommened step by step: bitToken_check(), bitToken_info(), bitToken_viz(), bitToken()
bitToken_check(example)

bitToken_info(chatGPT_news1, "title"", add=TRUE)

bitToken_viz(chatGPT_news1, "title"", type="histogram")

# Multi-core support functions

# Tokenize Text Column with Multi-Core Support
bitToken_m(chatGPT_news1, "title")

# Extract Token in Descending Order by Frequency with Multi-Core Support
bitToken_info_m(chatGPT_news1, "title")

bitToken_viz_m(chatGPT_news1, "title", info = TRUEE)
```

## **License**

This package is licensed under the GPL-3 License. See the LICENSE file for more information.
