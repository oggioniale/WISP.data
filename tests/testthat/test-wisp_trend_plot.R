library(testthat)

test_that("wisp_trend_plot: validation of temporal trends and aggregation", {
  
  # CSV upload
  path_sr <- "reflectance_data_sr_20240914.csv"
  if (!file.exists(path_sr)) {
    skip("Test CSV file (SR) not found")
  }
  
  df_sr <- read.csv(path_sr, check.names = FALSE)
  
  # Test: return of the long dataframe
  suppressMessages({
    df_long <- wisp_trend_plot(
      data = df_sr,
      params = c("TSM", "Chla"),
      aggregate = "none",
      return_long_df = TRUE
    )
  })
  
  expect_s3_class(df_long, "data.frame")
  expect_equal(nrow(df_long), nrow(df_sr) * 2)
  
  # Test: error if “none” is requested with multiple days
  df_multi_day <- df_sr[c(1, 1), ]
  df_multi_day$measurement.date[2] <- "2024-09-15T10:00:00.000Z"
  
  suppressMessages({
    expect_error(
      wisp_trend_plot(df_multi_day, aggregate = "none"),
      "requesting multiple days"
    )
  })
  
  # Test: daily Mean Aggregation
  suppressMessages({
    fig_trend <- wisp_trend_plot(
      data = df_multi_day,
      params = c("TSM", "Novoa_SPM"),
      aggregate = "daily_mean",
      merge_plot = TRUE
    )
  })
  
  expect_s3_class(fig_trend, "plotly")
  expect_match(fig_trend$x$layout$title$text, "Time trend", fixed = TRUE)
  expect_true(!is.null(fig_trend$x$layout$xaxis))
  
  # Test: error "merge" with different units of measurements
  suppressMessages({
    expect_error(
      wisp_trend_plot(df_sr, params = c("TSM", "Chla"), merge_plot = TRUE),
      "do not have any common units"
    )
  })
  
  # Test: custom colour management
  my_cols <- c("#FF0000", "#00FF00")
  suppressMessages({
    fig_cols <- wisp_trend_plot(
      data = df_sr,
      params = c("TSM", "Chla"),
      colors = my_cols
    )
  })
  
  trace_colors <- sapply(fig_cols$x$data, function(x) x$line$color)
  expect_true(any(grepl("FF0000", trace_colors, ignore.case = TRUE) | 
                    grepl("255,0,0", trace_colors)))
})