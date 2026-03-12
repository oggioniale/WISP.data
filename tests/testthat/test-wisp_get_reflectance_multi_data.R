library(testthat)
library(httptest2)

# Test: multi-day aggregation with data
test_that("aggregates data correctly over multiple days", {
  temp_dir <- file.path(tempdir(), "test_multi_data")
  with_mock_dir("mock_reflectance_data_multi_1", {
    suppressWarnings({
      res <- wisp_get_reflectance_multi_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-02T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy",
        save_csv  = TRUE,
        out_dir   = temp_dir
      )
    })
    expect_s3_class(res, "tbl_df")
    expect_true(nrow(res) > 0)
    expect_true(dir.exists(temp_dir))
    expected_file <- file.path(temp_dir, "reflectance_data_20240801_20240802.csv")
    expect_true(file.exists(expected_file))
  })
  
  unlink(temp_dir, recursive = TRUE)
})

# Test: returns NULL when no data exists
test_that("handles empty results gracefully", {
  with_mock_dir("mock_no_data", {
    res_empty <- suppressMessages(
      wisp_get_reflectance_multi_data(
        time_from = "2024-09-01T09:00",
        time_to   = "2024-09-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy"
      )
    )
    expect_s3_class(res_empty, "tbl_df")
    expect_equal(nrow(res_empty), 0)
  })
})

# Test: multi-day aggregation with data and no data
test_that("combines multi-day data correctly even with missing days", {
  with_mock_dir("mock_reflectance_data_multi_2", {
    res_mixed <- suppressWarnings(suppressMessages(
      wisp_get_reflectance_multi_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-10T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy"
      )
    ))
    expect_s3_class(res_mixed, "tbl_df")
    expect_true(nrow(res_mixed) > 0)
    unique_dates <- unique(as.Date(res_mixed$measurement.date))
    expect_contains(as.character(unique_dates), "2024-08-01")
  })
})