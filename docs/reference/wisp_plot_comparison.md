# Comparison plot of Raw vs QC vs SR reflectance data

**\[stable\]** This function creates an interactive side-by-side visual
comparison of the different WISPstation data processing levels. Using
plotly submodules, it enables the visualization of up to three aligned
plots within a single interactive window: raw data downloaded directly
from WISPstation, processed data after QC, processed data after SR. This
provides a powerful tool for visually assessing how filtering and
correction algorithms modify spectral signatures, remove artifacts, and
improve data quality.

## Usage

``` r
wisp_plot_comparison(
  raw_data,
  qc_data = NULL,
  sr_data = NULL,
  raw_args = NULL,
  qc_args = NULL,
  sr_args = NULL
)
```

## Arguments

- raw_data:

  A `tibble`. The original data obtained by
  [`wisp_get_reflectance_data()`](https://github.com/oggioniale/WISP.data/reference/wisp_get_reflectance_data.md).

- qc_data:

  A `tibble`. The data after
  [`wisp_qc_reflectance_data()`](https://github.com/oggioniale/WISP.data/reference/wisp_qc_reflectance_data.md)
  operations. Default is `NULL`.

- sr_data:

  A `tibble`. The data after
  [`wisp_sr_reflectance_data()`](https://github.com/oggioniale/WISP.data/reference/wisp_sr_reflectance_data.md)
  operations. Default is `NULL`.

- raw_args:

  A `list` of arguments to be passed to `wisp_plot_reflectance_data` for
  the raw data plot (legend). Default is `NULL`.

- qc_args:

  A `list` of arguments to be passed to `wisp_plot_reflectance_data` for
  the QC data plot (legend). Default is `NULL`.

- sr_args:

  A `list` of arguments to be passed to `wisp_plot_reflectance_data` for
  the SR data plot (legend). Default is `NULL`.

## Value

A `plotly` subplot object comparing the spectral signatures. If only
`raw_data` is provided or valid, a single plot is returned.

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) { # \dontrun{
## Not run:
custom_raw <- list(legend_TSM = FALSE, legend_Chla = FALSE)
custom_qc <- list(legend_TSM = TRUE, legend_Chla = TRUE, legend_Kd = FALSE)
custom_sr <- list(legend_TSM = TRUE, legend_mishra_CHL = FALSE)

wisp_plot_comparison(
  raw_data = reflect_data,
  qc_data  = reflect_data_qc,
  sr_data  = reflect_data_sr,
  raw_args = custom_raw,
  qc_args  = custom_qc,
  sr_args  = custom_sr
)
print(fig_comparison)
} # }
## End (Not run)
```
