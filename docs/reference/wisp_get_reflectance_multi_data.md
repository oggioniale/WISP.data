# Get data of reflectance (level2) from WISPstation for multiple dates

**\[stable\]** This function acts as an iterative wrapper around
[`wisp_get_reflectance_data()`](https://github.com/oggioniale/WISP.data/reference/wisp_get_reflectance_data.md).
It is specifically designed to download extended time series that exceed
the limits of a single API request. The function automatically splits
the user-defined time interval into daily blocks, performs sequential
downloads, and aggregates all retrieved data into a single, coherent
tibble.

## Usage

``` r
wisp_get_reflectance_multi_data(
  version = "1.0",
  time_from = NULL,
  time_to = NULL,
  station = NULL,
  userid = NULL,
  pwd = NULL,
  save_csv = FALSE,
  out_dir = "outputs"
)
```

## Arguments

- version:

  A `character`. It is the version of the API data. Default is "1.0"

- time_from:

  A `character`. It is the date and time from which the data is
  requested.

- time_to:

  A `character`. It is the date and time to which the data is requested.

- station:

  A `character`. It is the name of the station.

- userid:

  A `character`. It is the userid to access to the data service.

- pwd:

  A `character`. It is the password to access to the data service.

- save_csv:

  A `logical`. If `TRUE`, the function saves the reflectance data.

- out_dir:

  A `character`. The directory where the CSV file will be saved. Default
  is "outputs" within the working directory.

## Value

A `tibble` with measurement id, measurement date, instrument name,
level2_quality, set of sensor (irradiance and radiances), waterquality
values of TSM (Van Der Woerd & Pasterkamp, 2008), Chla (Gons et al.,
2005), Kd, and cpc as provided by instrument by default, all the
reflectance values from 350 to 900 nm.

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) { # \dontrun{
## Not run:
reflect_data <- wisp_get_reflectance_multi_data(
  time_from = "2024-04-08T09:00",
  time_to = "2024-04-10T14:00",
  station = "WISPstation012",
  userid = userid,
  pwd = pwd,
  save_csv = FALSE,
  out_dir = "outputs"
)

# NA data on 2024-09-01
reflect_data <- wisp_get_reflectance_multi_data(
  time_from = "2024-08-31T09:00",
  time_to = "2024-09-02T14:00",
  station = "WISPstation012",
  userid = userid,
  pwd = pwd,
  save_csv = FALSE,
  out_dir = "outputs"
)
} # }
## End (Not run)
```
