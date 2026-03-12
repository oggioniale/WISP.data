library(testthat)

test_that("wisp_plot_reflectance_data: validation with real SR dataset", {

  # CSV upload
  path <- "reflectance_data_sr_20240914.csv"
  skip_if_not(file.exists(path), message = "Test file not found")
  
  df_real <- read.csv(path, check.names = FALSE) |> dplyr::as_tibble()
  
  # Test with multiple bio-optical parameters activated
  expect_no_error({
    p <- wisp_plot_reflectance_data(
      data = df_real,
      legend_TSM            = TRUE, 
      legend_Chla           = TRUE, 
      legend_Kd             = TRUE, 
      legend_cpc            = TRUE, 
      legend_scatt          = TRUE,
      legend_ratio          = TRUE,
      legend_novoa_SPM      = TRUE,
      legend_novoa_TUR      = TRUE,
      legend_jiang_TSS      = TRUE,
      legend_gons_CHL       = TRUE,
      legend_gons740_CHL    = TRUE,
      legend_NDCI           = TRUE,
      legend_mishra_CHL     = TRUE,
      legend_hue_angle      = TRUE, 
      legend_dom_wavelength = TRUE,
      legend_OWT_class      = TRUE,
      legend_OWT_score      = TRUE,
      legend_OWT_z_dist     = TRUE
    )
  })
  
  # Checks on the Plotly object
  expect_s3_class(p, "plotly")
  plot_data <- plotly::plotly_build(p)$x$data
  first_trace <- plot_data[[1]]
  expect_true(min(first_trace$x) >= 350) 
  expect_true(max(first_trace$x) <= 900)
})

test_that("wisp_plot_reflectance_data: edge cases with real data columns", {
  path <- "reflectance_data_sr_20240914.csv"
  skip_if_not(file.exists(path))
  
  df_real <- read.csv(path, check.names = FALSE) |> dplyr::as_tibble()
  
  p_single <- wisp_plot_reflectance_data(df_real[1, ])
  expect_s3_class(p_single, "plotly")
  
  # Aggiunto units:: e dplyr::
  df_with_units <- df_real[1:2, ] |>
    dplyr::mutate(waterquality.tsm = units::set_units(waterquality.tsm, "g/m^3"))
  
  expect_no_error({
    wisp_plot_reflectance_data(df_with_units, legend_TSM = TRUE)
  })
})