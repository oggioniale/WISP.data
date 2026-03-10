library(httptest2)

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

test_that("wisp_get_reflectance_data creates output directory and processes data", {
  temp_dir <- file.path(tempdir(), "test_joss_folder")
  
  with_mock_dir("wisp_api_mock", {
    suppressWarnings({
      res <- wisp_get_reflectance_data(
        time_from = "2024-08-01T09:00",
        time_to   = "2024-08-01T14:00",
        station   = "WISPstation012",
        save_csv  = TRUE,      
        out_dir   = temp_dir,  
        userid    = "dummy",
        pwd       = "dummy"
      )
    })
    
    expect_s3_class(res, "tbl_df")
    expect_true(dir.exists(temp_dir))
    expect_true(length(list.files(temp_dir)) > 0)
  })
  
  unlink(temp_dir, recursive = TRUE)
})