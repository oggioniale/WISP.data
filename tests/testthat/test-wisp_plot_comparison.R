library(testthat)

test_that("wisp_plot_comparison: structural check for multi-panel plotly output", {
  
  # CSV upload
  path_raw <- "reflectance_data_20240914.csv"
  path_qc  <- "reflectance_data_qc_20240914.csv"
  path_sr  <- "reflectance_data_sr_20240914.csv"
  
  if (!all(file.exists(c(path_raw, path_qc, path_sr)))) {
    skip("Test CSV files not found in the directory")
  }
  
  # Data preparation
  read_and_unitize <- function(path) {
    df <- read.csv(path, check.names = FALSE)
    df |> dplyr::mutate(dplyr::across(dplyr::starts_with("nm_"), ~units::set_units(.x, "1/sr")))
  }
  
  df_raw <- read_and_unitize(path_raw)
  df_qc  <- read_and_unitize(path_qc)
  df_sr  <- read_and_unitize(path_sr)
  
  # Function execution
  suppressMessages({
    fig_full <- wisp_plot_comparison(
      raw_data = df_raw,
      qc_data  = df_qc,
      sr_data  = df_sr,
      raw_args = list(legend_TSM = FALSE),
      qc_args  = list(legend_NDCI = TRUE)
    )
  })
  
  # Assertions
  expect_s3_class(fig_full, "plotly")
  expect_s3_class(fig_full, "htmlwidget")
  
  expect_true(length(fig_full$x$data) > 0)
 
  expect_match(fig_full$x$layout$title$text, "WISPstation", fixed = TRUE)
  
  expect_true(!is.null(fig_full$x$layout$xaxis))
  expect_true(!is.null(fig_full$x$layout$xaxis2))
  expect_true(!is.null(fig_full$x$layout$xaxis3))

  expect_match(fig_full$x$layout$xaxis$title$text, "Wavelength", fixed = TRUE)
  expect_match(fig_full$x$layout$yaxis$title$text, "Rrs", fixed = TRUE)
 
  suppressMessages({
    fig_single <- wisp_plot_comparison(raw_data = df_raw)
  })
  
  expect_match(fig_single$x$layout$title$text, "WISPstation", fixed = TRUE)
  expect_null(fig_single$x$layout$xaxis2)
  
  df_qc_empty <- df_qc[0, ]
  suppressMessages({
    fig_empty_qc <- wisp_plot_comparison(raw_data = df_raw, qc_data = df_qc_empty)
  })
 
  expect_match(fig_empty_qc$x$layout$title$text, "WISPstation", fixed = TRUE)
})