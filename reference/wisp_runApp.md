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
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `launch.browser`, `port`, `host`).

## Value

No return value, called for side effects to launch the interactive Shiny
application.

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

## Examples

``` r
if (interactive()) {
  # Launch the Shiny application
  wisp_runApp(launch.browser = TRUE)
}
```
