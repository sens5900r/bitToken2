#' News dataset for bitToken2 package
#'
#' A dataset containing 20 rows of news article titles and categories, designed for use with the bitToken2 package.
#'
#' @name chatGPT_news1
#' @aliases chatGPT_news1
#' @rdname chatGPT_news1
#' @export
# Create a dataframe with 20 rows
chatGPT_df1 <- data.frame(
  title = c(
    "New policy proposal aims to reduce carbon emissions",
    "Opposition leader calls for investigation into government corruption",
    "Government announces plans to increase funding for healthcare",
    "Economic growth slows down in Q3 of 2022",
    "Global stock markets experience sharp decline",
    "Report shows rising income inequality in the US",
    "Rise in mental health concerns amidst pandemic",
    "Study finds correlation between social media use and depression",
    "Calls for greater support for domestic abuse victims",
    "Apple unveils new lineup of iPhones at annual event",
    "Facebook faces criticism over handling of user data",
    "Google announces new privacy features for users",
    "UN report highlights increasing climate change risks",
    "China launches new space station module into orbit",
    "Millions affected by ongoing refugee crisis in Syria",
    "Brexit negotiations stall as deadline approaches",
    "WHO declares COVID-19 a global pandemic",
    "Amazon faces antitrust scrutiny from regulators",
    "Young people protest for climate action around the world",
    "Racism in football under the spotlight after European Championship final"
  ),
  category = c(
    "politics", "politics", "politics",
    "economy", "economy", "economy",
    "society", "society", "society",
    "IT", "IT", "IT",
    "world", "world", "world",
    "politics", "world", "IT",
    "world", "society"
  ),
  stringsAsFactors = FALSE
)

# Assign name to the dataframe
chatGPT_news1 <- chatGPT_df1


data(chatGPT_news1)
