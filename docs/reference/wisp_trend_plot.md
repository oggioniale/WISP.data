# Creates a temporal trend plot of one or more water quality parameters

**\[stable\]** This function generates interactive temporal plots for
one or more parameters. It is designed to handle both high-frequency
measurements within a single day and long-term time series spanning
multiple months. The function can aggregate data (daily mean or median)
and allows comparison of multiple parameters in the same plot if they
share the same unit of measurement.

## Usage

``` r
wisp_trend_plot(
  data,
  params = c("TSM", "Chla"),
  datetime_col = "measurement.date",
  instrument_col = "instrument.name",
  aggregate = c("none", "daily_mean", "daily_median"),
  merge_plot = FALSE,
  na.rm = TRUE,
  colors = NULL,
  title = NULL,
  return_long_df = FALSE
)
```

## Arguments

- data:

  A `tibble` containing water quality parameters and spectral
  signatures.

- params:

  A character vector specifying which parameters to plot. Default is
  `c("TSM", "Chla")`.

- datetime_col:

  A `character`. Name of the column with datetime values. Default is
  `"measurement.date"`.

- instrument_col:

  A `character`. Name of the column with instrument identifiers. Default
  is `"instrument.name"`.

- aggregate:

  A `character` specifying whether to aggregate data. Options are:

  - `"none"`: Plots all available values (requires data for only one
    day).

  - `"daily_mean"`: Calculates and plots the daily average, including a
    ribbon for the Standard Deviation (SD) (requires data for multiple
    days).

  - `"daily_median"`: Calculates and plots the daily median (requires
    data for multiple days).

  Default is `"none"`.

- merge_plot:

  A `logical`. If `TRUE`, parameters that share the same unit of
  measurement will be merged into a single plot. The function will throw
  an error if no common units are found among the requested `params`.
  Default is `FALSE`.

- na.rm:

  A `logical`. If `TRUE`, NA values are ignored during aggregation.
  Default is `TRUE`.

- colors:

  A `character` vector of colors for each parameter. Default uses
  `viridis` palette.

- title:

  A `character`. Optional title for the plot. Default is `NULL`.

- return_long_df:

  A `logical`. If `TRUE`, the function returns the long format dataframe
  used for plotting instead of the `plotly` object. Default is `FALSE`.

## Value

An interactive `plotly` object showing the temporal trend of the
selected parameters, with optional ribbons for standard deviation.

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, PhD <nicola.ghirardi@cnr.it>

## Examples

``` r
# Example usage
if (FALSE) { # \dontrun{
# Standard plot with facets for each parameter
fig_trend <- wisp_trend_plot(
   data = reflect_data_sr,
   params = c("TSM", "Chla"),
   aggregate = "none",
   merge_plot = FALSE
)
print(fig_trend)

# Merged plot for parameters with common units 
fig_merged <- wisp_trend_plot(
   data = reflect_data_sr,
   params = c("TSM", "Novoa_SPM"),
   aggregate = "daily_mean",
   merge_plot = TRUE
)
print(fig_trend)
} # }
```
