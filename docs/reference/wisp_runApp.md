# Run shiny app for get and visualize WISP data

**\[experimental\]** This function runs the Shiny app for querying and
visualizing data from a specific WISP station.

## Usage

``` r
wisp_runApp(stations = c("WISPstation012", "WISPstation013"), ...)
```

## Arguments

- stations:

  A `character vector` of station names.

- ...:

  Other parameters passed to
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
WISP.data::wisp_runApp(launch.browser = rstudioapi::viewer)
} # }
```
