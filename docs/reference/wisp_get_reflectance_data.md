# Get data of reflectance (level2) from WISPstation for a specific date

**\[stable\]** This function represents the main entry point for data
acquisition within the WISP.data package. It connects to the official
Water Insight APIs to download spectral reflectance measurements for a
user-defined time interval. The function handles authentication queries
the remote server and returns the retrieved data in a structured tibble
format. In addition to hyperspectral reflectance data (350-900 nm), the
function also retrieves the water quality parameters natively computed
by the WISPstation, including: TSM (Van Der Woerd & Pasterkamp, 2008),
Chla (Gons et al., 2005), Kd (Gons et al., 1998), and cpc (Simis, 2006).

## Usage

``` r
wisp_get_reflectance_data(
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

  A `character`. It is the version of the API data. Default is "1.0".

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
values of TSM, Chla, Kd, and cpc as provided by instrument by default,
all the reflectance values from 350 to 900 nm.

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) { # \dontrun{
## Not run:
# NA data
reflect_data <- wisp_get_reflectance_data(
  time_from = "2024-09-01T09:00",
  time_to = "2024-09-01T14:00",
  station = "WISPstation012",
  userid = userid,
  pwd = pwd,
  save_csv = FALSE,
  out_dir = "outputs"
)

# with data
reflect_data <- wisp_get_reflectance_data(
  time_from = "2024-08-01T09:00",
  time_to = "2024-08-01T14:00",
  station = "WISPstation012",
  userid = userid,
  pwd = pwd,
  save_csv = FALSE,
  out_dir = "outputs"
)

# no data for the station selected
reflect_data <- wisp_get_reflectance_data(
  time_from = "2019-06-20T09:00",
  time_to = "2019-06-20T14:00",
  station = "WISPstation012",
  userid = userid,
  pwd = pwd,
  save_csv = FALSE,
  out_dir = "outputs"
)

# The two dates are not consistent
reflect_data <- wisp_get_reflectance_data(
  time_from = "2019-06-20T09:00",
  time_to = "2020-06-20T14:00",
  station = "WISPstation012",
  userid = userid,
  pwd = pwd,
  save_csv = FALSE,
  out_dir = "outputs"
)
} # }
## End (Not run)
```
