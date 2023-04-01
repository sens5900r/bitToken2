# **bitToken2**

bitToken2 is an R package for tokenizing text data in a data frame. It provides a simple interface for splitting text data into tokens, which can be useful for natural language processing tasks like sentiment analysis, topic modeling, and text classification.

## **Installation**

You can install bitToken2 from GitHub using the **`devtools`** package:

```{r}
devtools::install_github("sens5900r/bitToken2")
```

## **Usage**

Here's an example of how to use bitToken2 to tokenize text data in a data frame:

```{r}
library(bitToken2)

# Load example data
data("chatGPT_news1")

# Tokenize text data in the "title" column
tokens <- bitToken(data = chatGPT_news1, text_column = "title")

# View the first 5 tokens
head(tokens)

# recommened step by step: bitToken_check(), bitToken_info(), bitToken_viz(), bitToken()
```

## **License**

This package is licensed under the GPL-3 License. See the LICENSE file for more information.
