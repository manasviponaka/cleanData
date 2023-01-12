raw_data <- read.csv("https://raw.githubusercontent.com/manasviponaka/cleanData/main/insurance.csv")
test_that("changing character columns to factors works", {

  returned_value <- cleaning(raw_data)

  raw_data[sapply(raw_data, is.character)] <-data.frame(lapply(raw_data[sapply(raw_data, is.character)],as.factor))

  expected_value <- raw_data

  expect_equal(returned_value, expected_value)
})
test_that("Checking error for give inputs that are not dataframes", {

  returned_value <- cleaning(NULL)


  expected_value <- "error - The function takes only data.frame class obejects as input"

  expect_equal(returned_value, expected_value)
})




