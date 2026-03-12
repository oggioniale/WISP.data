library(testthat)
library(httptest2)

# Test: returns NULL if dates are inconsistent
test_that("wisp_get_reflectance_data handles inconsistent dates", {
  expect_message(
    res <- wisp_get_reflectance_data(
      time_from = "2024-09-01T09:00",
      time_to   = "2024-09-02T14:00"
    ),
    "The two dates are not consistent"
  )
  expect_null(res)
})

# Test: returns NULL when no data exists
test_that("returns NULL when no data exist for the requested date", {
  with_mock_dir("mock_no_data", {
    expect_message(
      res <- wisp_get_reflectance_data(
        time_from = "2024-09-01T09:00",
        time_to   = "2024-09-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy"
      ),
      "instrument does not acquire data"
    )
    expect_null(res)   
  })
})

# Test: returns a tibble when data is available
test_that("returns tibble when data are available", {
  with_mock_dir("mock_reflectance_data", {
    res <- suppressWarnings(
      wisp_get_reflectance_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy",
        save_csv  = FALSE
      )
    )
    expect_s3_class(res, "tbl_df")
    expect_true(nrow(res) > 0)
  })
})

# Test: check the tibble structure
test_that("output structure contains expected columns", {
  with_mock_dir("mock_reflectance_data", {
    res <- suppressWarnings(
      wisp_get_reflectance_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy",
        save_csv  = FALSE
      )
    )
    expect_true("measurement.id" %in% names(res))
    expect_true("measurement.date" %in% names(res))
    expect_true("instrument.name" %in% names(res))
    expect_true("nm_350" %in% names(res))
    expect_true("nm_900" %in% names(res))
  })
})

# Test: check CSV output (TRUE)
test_that("save_csv creates output directory and file", {
  temp_dir <- file.path(tempdir(), "test_wisp_output")
  with_mock_dir("mock_reflectance_data", {
    suppressWarnings({
      res <- wisp_get_reflectance_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy",
        save_csv  = TRUE,
        out_dir   = temp_dir
      )
    })
    expect_true(dir.exists(temp_dir))
    expected_file <- file.path(
      temp_dir,
      "reflectance_data_2024-08-01.csv"
    )
    expect_true(file.exists(expected_file))
  })
  unlink(temp_dir, recursive = TRUE)
})

# Test: check no CSV output (FALSE)
test_that("no csv is created when save_csv is FALSE", {
  temp_dir <- file.path(tempdir(), "test_wisp_no_csv")
  with_mock_dir("mock_reflectance_data", {
    res <- suppressWarnings(
      wisp_get_reflectance_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy",
        save_csv  = FALSE,
        out_dir   = temp_dir
      )
    )
    expect_s3_class(res, "tbl_df")
    expect_false(dir.exists(temp_dir))
  })
})