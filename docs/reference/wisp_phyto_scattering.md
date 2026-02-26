# Phytoplankton Scattering for WISPstation reflectance data

**\[experimental\]** This function calculates the reflectance peak due
to phytoplankton scattering (690-710 nm) and calculates the ratio of the
latter to the second chlorophyll absorption peak (670-680 nm).
\`calc_scatt\`\`\` can be either TRUE (default) or FALSE.

## Usage

``` r
wisp_phyto_scattering(qc_data, sr_data, calc_scatt = TRUE)
```

## Arguments

- qc_data:

  A `tibble` from wisp_qc_reflectance_data() function.

- sr_data:

  A `tibble` from wisp_sr_reflectance_data() function.

- calc_scatt:

  A `logical`. If `TRUE`, the function calculates the two parameters.
  Default is `TRUE`.

## Value

The result is a list with two elements: ...

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) {
## Not run:

# Calculate the two parameters
scattering_results <- WISP.data::wisp_phyto_scattering(qc_data = reflect_data_qc, sr_data = reflect_data_sr, calc_scatt = T)
reflect_data_qc <- scattering_results$qc_data
reflect_data_sr <- scattering_results$sr_data

# Don't calculate the two parameters
scattering_results <- WISP.data::wisp_phyto_scattering(qc_data = reflect_data_qc, sr_data = reflect_data_sr, calc_scatt = F)
reflect_data_qc <- scattering_results$qc_data
reflect_data_sr <- scattering_results$sr_data

}
## End (Not run)
```
