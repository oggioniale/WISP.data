# Create a plot of reflectance data

**\[stable\]** This function generates an interactive visualization of
all spectral signatures contained in a dataset, based on the plotly
library. It is highly flexible and can be used to display: raw data
downloaded directly from WISPstation, processed data after QC, processed
data after SR. The function’s distinctive feature is its dynamic tooltip
system: when hovering over a spectral curve, users can instantly
visualize the corresponding acquisition date and time, together with all
associated bio-optical parameters computed for that specific
measurement.

## Usage

``` r
wisp_plot_reflectance_data(
  data,
  legend_TSM = TRUE,
  legend_Chla = TRUE,
  legend_Kd = TRUE,
  legend_cpc = TRUE,
  legend_scatt = FALSE,
  legend_ratio = FALSE,
  legend_novoa_SPM = FALSE,
  legend_novoa_TUR = FALSE,
  legend_jiang_TSS = FALSE,
  legend_gons_CHL = FALSE,
  legend_gons740_CHL = FALSE,
  legend_NDCI = FALSE,
  legend_mishra_CHL = FALSE,
  legend_hue_angle = FALSE,
  legend_dom_wavelength = FALSE,
  legend_OWT_class = FALSE,
  legend_OWT_score = FALSE,
  legend_OWT_z_dist = FALSE
)
```

## Arguments

- data:

  A `tibble` obtained by any of the functions provided by this package:
  [`wisp_get_reflectance_data()`](https://github.com/oggioniale/WISP.data/reference/wisp_get_reflectance_data.md),
  or after QC and SR removal operations.

- legend_TSM:

  A `logical`. If `TRUE`, the plot legend includes the `TSM` values.
  Default is `TRUE`.

- legend_Chla:

  A `logical`. If `TRUE`, the plot legend includes the `Chla` values.
  Default is `TRUE`.

- legend_Kd:

  A `logical`. If `TRUE`, the plot legend includes the `Kd` values.
  Default is `TRUE`.

- legend_cpc:

  A `logical`. If `TRUE`, the plot legend includes the `cpc` values.
  Default is `TRUE`.

- legend_scatt:

  A `logical`. If `TRUE`, the plot legend includes the `scattering`
  values. Default is `FALSE`.

- legend_ratio:

  A `logical`. If `TRUE`, the plot legend includes the `ratio` values.
  Default is `FALSE`.

- legend_novoa_SPM:

  A `logical`. If `TRUE`, the plot legend includes the
  `Novoa_SPM`values. Default is `FALSE`.

- legend_novoa_TUR:

  A `logical`. If `TRUE`, the plot legend includes the
  `Novoa_TUR`values. Default is `FALSE`.

- legend_jiang_TSS:

  A `logical`. If `TRUE`, the plot legend includes the
  `Jiang_TSS`values. Default is `FALSE`.

- legend_gons_CHL:

  A `logical`. If `TRUE`, the plot legend includes the `Gons_CHL`values.
  Default is `FALSE`.

- legend_gons740_CHL:

  A `logical`. If `TRUE`, the plot legend includes the
  `Gons740_CHL`values. Default is `FALSE`.

- legend_NDCI:

  A `logical`. If `TRUE`, the plot legend includes the `NDCI`values.
  Default is `FALSE`.

- legend_mishra_CHL:

  A `logical`. If `TRUE`, the plot legend includes the
  `Mishra_CHL`values. Default is `FALSE`.

- legend_hue_angle:

  A `logical`. If `TRUE`, the plot legend includes the
  `Hue_Angle`values. Default is `FALSE`.

- legend_dom_wavelength:

  A `logical`. If `TRUE`, the plot legend includes the
  `Dominant_Wavelength`values. Default is `FALSE`.

- legend_OWT_class:

  A `logical`. If `TRUE`, the plot legend includes the `OWT_class`.
  Default is `FALSE`.

- legend_OWT_score:

  A `logical`. If `TRUE`, the plot legend includes the `OWT_score`
  (membership grade). Default is `FALSE`.

- legend_OWT_z_dist:

  A `logical`. If `TRUE`, the plot legend includes the `OWT_z_dist`
  (statistical distance). Default is `FALSE`.

## Value

an interactive plot showing the spectral signatures of the reflectance
data.

## Author

Alessandro Oggioni, phD <oggioni.a@irea.cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) { # \dontrun{
## Not run:
wisp_plot_reflectance_data(
  data = reflect_data_sr,
  legend_TSM = TRUE,
  legend_Chla = TRUE,
  legend_Kd = TRUE,
  legend_cpc = TRUE,
  legend_scatt = FALSE,
  legend_ratio = FALSE,
  legend_novoa_SPM = FALSE,
  legend_novoa_TUR = FALSE,
  legend_jiang_TSS = FALSE,
  legend_gons_CHL  = FALSE,
  legend_gons740_CHL = FALSE,
  legend_NDCI = FALSE,
  legend_mishra_CHL = FALSE,
  legend_hue_angle = FALSE, 
  legend_dom_wavelength = FALSE,
  legend_OWT_class = FALSE,
  legend_OWT_score = FALSE,
  legend_OWT_z_dist = FALSE  
)
} # }
## End (Not run)
```
