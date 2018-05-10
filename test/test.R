library(testthat)




fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

dt <- fars_read("accident_2013.csv.bz2")

dt %>%
  expect_type("list") %>%
  expect_s3_class("data.frame") %>%
  expect_length(50)
