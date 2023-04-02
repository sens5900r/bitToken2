context("bitToken")

test_that("bitToken returns a data frame", {
  data <- data.frame(text = c("This is a sample text", "Another sample text"))
  result <- bitToken(data = data, text_column = "text")
  expect_that(result, is_a("list"))
  expect_that(result[[1]], is_a("character"))
})

test_that("bitToken returns lengths of tokens when lengths parameter is set to TRUE", {
  data <- data.frame(text = c("This is a sample text", "Another sample text"))
  result <- bitToken(data = data, text_column = "text", lengths = TRUE)
  expect_that(result, is_a("numeric"))
})
