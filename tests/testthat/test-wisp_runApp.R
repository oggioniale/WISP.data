test_that(".wisp_valid_date_range accepts a same-day range", {
  expect_true(.wisp_valid_date_range("2024-09-01", "2024-09-01"))
})

test_that(".wisp_valid_date_range accepts date_to after date_from", {
  expect_true(.wisp_valid_date_range("2024-09-01", "2024-09-05"))
})

test_that(".wisp_valid_date_range rejects date_to before date_from", {
  expect_false(.wisp_valid_date_range("2024-09-05", "2024-09-01"))
})

test_that(".wisp_valid_date_range works with Date objects", {
  expect_true(.wisp_valid_date_range(as.Date("2024-09-01"), as.Date("2024-09-02")))
  expect_false(.wisp_valid_date_range(as.Date("2024-09-02"), as.Date("2024-09-01")))
})

test_that(".wisp_build_time_range combines date and hour correctly", {
  expect_equal(
    .wisp_build_time_range("2024-09-01", "09:00"),
    "2024-09-01T09:00"
  )
  expect_equal(
    .wisp_build_time_range(as.Date("2024-09-01"), "17:00"),
    "2024-09-01T17:00"
  )
})

test_that(".wisp_build_download_filename uses 'sr' suffix when SR is applied", {
  fname <- .wisp_build_download_filename(
    station   = "WISPstation012",
    date_from = "2024-09-01",
    date_to   = "2024-09-01",
    do_sr     = TRUE,
    do_qc     = TRUE
  )
  expect_equal(fname, "wisp_reflectance_WISPstation012_2024-09-01_2024-09-01_sr.csv")
})

test_that(".wisp_build_download_filename uses 'qc' suffix when only QC is applied", {
  fname <- .wisp_build_download_filename(
    station   = "WISPstation012",
    date_from = "2024-09-01",
    date_to   = "2024-09-02",
    do_sr     = FALSE,
    do_qc     = TRUE
  )
  expect_equal(fname, "wisp_reflectance_WISPstation012_2024-09-01_2024-09-02_qc.csv")
})

test_that(".wisp_build_download_filename uses 'raw' suffix when neither QC nor SR is applied", {
  fname <- .wisp_build_download_filename(
    station   = "WISPstation013",
    date_from = "2024-09-01",
    date_to   = "2024-09-01",
    do_sr     = FALSE,
    do_qc     = FALSE
  )
  expect_equal(fname, "wisp_reflectance_WISPstation013_2024-09-01_2024-09-01_raw.csv")
})
