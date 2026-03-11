library(testthat)
library(dplyr)
library(units)

test_that("wisp_sr_reflectance_data: validation with real QC vs SR datasets", {
  
  # CSV upload
  path_qc <- "reflectance_data_qc_20240501.csv"
  path_sr <- "reflectance_data_sr_20240501.csv"
  
  if (!file.exists(path_qc) || !file.exists(path_sr)) {
    skip("Test CSV files not found in the directory")
  }
  
  df_input_raw <- read.csv(path_qc, check.names = FALSE)
  df_expected_raw <- read.csv(path_sr, check.names = FALSE)
  
  df_input <- df_input_raw %>%
    mutate(across(starts_with("nm_"), ~set_units(.x, "1/sr")))
  
  # Function execution
  suppressMessages({
    res <- wisp_sr_reflectance_data(df_input, save_csv = FALSE)
  })
  
  actual_matrix <- res %>% 
    select(starts_with("nm_")) %>% 
    mutate(across(everything(), as.numeric)) %>% 
    as.matrix()
  
  expected_matrix <- df_expected_raw %>% 
    select(starts_with("nm_")) %>% 
    as.matrix()
  
  # Assertions
  expect_equal(dim(actual_matrix), dim(expected_matrix))
  expect_equal(actual_matrix, expected_matrix, tolerance = 1e-7)
  expect_true(all(res$SR == "corrected"))
  expect_true(all(res$delta > 0)) 
  expect_equal(res$measurement.id, df_expected_raw$measurement.id)
})