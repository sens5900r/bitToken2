#' @title Sample news article titles and categories
#' @description A dataset containing 30 news article titles and their corresponding categories
#' @keywords datasets
#' @export
chatGPT_news1 <- data.frame(
  title = c("New COVID-19 cases continue to rise in Europe",
            "The US imposes sanctions on Russia over cyberattacks",
            "Apple unveils new iPhone with advanced features",
            "South Korea imposes new regulations on cryptocurrency",
            "Global economy shows signs of recovery",
            "Artificial intelligence takes over jobs in manufacturing industry",
            "UK announces plans to reduce carbon emissions",
            "Chinese government cracks down on online gaming addiction",
            "North Korea launches new missile test",
            "The rise of electric cars in the automotive industry",
            "US withdraws troops from Afghanistan after 20-year war",
            "Japan announces new measures to boost economy",
            "France introduces new COVID-19 restrictions",
            "The impact of climate change on agriculture",
            "India surpasses China in population",
            "The future of work in a post-pandemic world",
            "Russia develops new military technology",
            "The benefits and drawbacks of social media",
            "South Africa struggles with high unemployment rate",
            "The rise of e-commerce in the retail industry",
            "UK faces Brexit trade deal uncertainty",
            "The effects of the COVID-19 pandemic on mental health",
            "China's Belt and Road Initiative",
            "The ethics of artificial intelligence",
            "The impact of the COVID-19 pandemic on education",
            "Germany sees rise in far-right extremism",
            "The future of renewable energy",
            "Iran's nuclear program",
            "The history of the internet",
            "The impact of the COVID-19 pandemic on travel"),
  category = c("world", "politics", "IT", "economy", "economy", "IT", "politics",
               "society", "world", "IT", "politics", "economy", "society",
               "world", "world", "society", "world", "society", "economy",
               "economy", "politics", "society", "world", "IT", "education",
               "society", "energy", "politics", "IT", "society")
)

# save the data as an .rda file
save(chatGPT_news1, file = "data/chatGPT_news1.rda")
