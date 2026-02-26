# Calculation of SPM concentration (Novoa et al., 2017)

**\[experimental\]** This function calculates the SPM concentration in
according to Novoa et al., 2017 and creates new columns in qc_data e
sr_data. `calc_SPM` can be either TRUE (default) or FALSE.

## Usage

``` r
wisp_novoa_SPM(qc_data, sr_data, calc_SPM = TRUE)
```

## Arguments

- qc_data:

  A `tibble` from wisp_qc_reflectance_data() function.

- sr_data:

  A `tibble` from wisp_sr_reflectance_data() function.

- calc_SPM:

  A `logical`. If `TRUE`, the function calculates the NOVOA SPM
  concentrations. Default is `TRUE`.

## Value

The result is a list with two elements: ...

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) {
#' ## Not run:

# Calculate NOVOA_SPM
novoa_SPM_results <- WISP.data::wisp_novoa_SPM(qc_data = reflect_data_qc, sr_data = reflect_data_sr, calc_SPM = T)
reflect_data_qc <- novoa_SPM_results$qc_data
reflect_data_sr <- novoa_SPM_results$sr_data

# Don't calculate NOVOA_SPM
novoa_SPM_results <- WISP.data::wisp_novoa_SPM(qc_data = reflect_data_qc, sr_data = reflect_data_sr, calc_SPM = F)
reflect_data_qc <- novoa_SPM_results$qc_data
reflect_data_sr <- novoa_SPM_results$sr_data
}
## End (Not run)
```
