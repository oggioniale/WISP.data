# Calculation of Turbidity (FNU) (Novoa et al., 2017)

**\[experimental\]** This function calculates the Turbidity (FNU) in
according to Novoa et al., 2017 and creates new columns in qc_data e
sr_data. `calc_TUR` can be either TRUE (default) or FALSE.

## Usage

``` r
wisp_novoa_TUR(qc_data, sr_data, calc_TUR = TRUE)
```

## Arguments

- qc_data:

  A `tibble` from wisp_qc_reflectance_data() function.

- sr_data:

  A `tibble` from wisp_sr_reflectance_data() function.

- calc_TUR:

  A `logical`. If `TRUE`, the function calculates the NOVOA TUR values
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

# Calculate NOVOA_SPM
novoa_TUR_results <- WISP.data::wisp_novoa_TUR(qc_data = reflect_data_qc, sr_data = reflect_data_sr, calc_TUR = T)
reflect_data_qc <- novoa_TUR_results$qc_data
reflect_data_sr <- novoa_TUR_results$sr_data

# Don't calculate NOVOA_SPM
novoa_TUR_results <- WISP.data::wisp_novoa_TUR(qc_data = reflect_data_qc, sr_data = reflect_data_sr, calc_TUR = F)
reflect_data_qc <- novoa_TUR_results$qc_data
reflect_data_sr <- novoa_TUR_results$sr_data
}
## End (Not run)
```
