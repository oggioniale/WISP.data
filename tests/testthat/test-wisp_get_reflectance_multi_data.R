library(httptest2)

test_that("wisp_get_reflectance_multi_data aggregates data correctly over multiple days", {
  temp_dir <- file.path(tempdir(), "test_multi_data")
  
  with_mock_dir("wisp_api_mock", {
    suppressWarnings(suppressMessages({
      res <- wisp_get_reflectance_multi_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy",
        save_csv  = TRUE,
        out_dir   = temp_dir
      )
    }))
    
    expect_s3_class(res, "tbl_df")
    expect_true(nrow(res) > 0)
    expect_true(dir.exists(temp_dir))
  })
  
  unlink(temp_dir, recursive = TRUE)
})

test_that("wisp_get_reflectance_multi_data handles empty results gracefully", {
  with_mock_dir("wisp_api_mock", {
    suppressMessages({
      res_empty <- wisp_get_reflectance_multi_data(
        time_from = "2024-09-01T09:00",
        time_to   = "2024-09-01T14:00",
        station   = "WISPstation012",
        userid    = "dummy",
        pwd       = "dummy"
      )
    })
    
    expect_equal(nrow(res_empty), 0)
  })
})