# Quality Control (QC) for WISPstation reflectance data

**\[stable\]** This function performs the Quality Control (QC) process
and applies different algorithms to spectral signatures. The function
applies a structured sequence of QC tests (QC1 - QC6), designed to
identify and remove low-quality or physically implausible spectra. In
addition, the function integrates independent quality assessment metrics
derived from the literature (QA and QWIP), providing robust spectral
validation through established optical criteria.

## Usage

``` r
wisp_qc_reflectance_data(
  data,
  maxPeak = 0.05,
  maxPeak_blue = 0.02,
  qa_threshold = 0.5,
  qwip_threshold = 0.2,
  calc_scatt = TRUE,
  calc_SPM = TRUE,
  calc_TUR = TRUE,
  calc_TSS = TRUE,
  calc_gons = TRUE,
  calc_gons740 = TRUE,
  calc_NDCI = TRUE,
  calc_mishra = TRUE,
  calc_dom_wave = TRUE,
  calc_OWT = TRUE,
  save_csv = FALSE,
  out_dir = "outputs"
)
```

## Arguments

- data:

  A `tibble`. From wisp_get_reflectance_data() function.

- maxPeak:

  A `decimal`. Maximum magnitude of the spectral signatures. We
  recommend setting this parameter to: 0.02 for clear and oligotrophic
  water, 0.05 for meso- to eutrophic water, and 0.08 for hypereutrophic
  and highly turbid water. Default is 0.05.

- maxPeak_blue:

  A `decimal`. Maximum magnitude 350 nm values. We recommend setting
  this parameter to: 0.02 (default).

- qa_threshold:

  A `decimal`. Minimum threshold for Quality Assurance (QA). We
  recommend setting this parameter to: 0.5 (default). To make QC more
  stringent, raise the threshold.

- qwip_threshold:

  A `decimal`. Maximum threshold for Quality Water Index Polynomial
  (QWIP). We recommend setting this parameter to: 0.2 (default). To make
  QC more stringent, decrease the threshold.

- calc_scatt:

  A `logical`. If `TRUE`, the function calculates the peak due to
  phytoplankton scattering (690-710 nm) and the ratio of the latter to
  the second chlorophyll absorption peak (670-680 nm). Default is
  `TRUE`.

- calc_SPM:

  A `logical`. If `TRUE`, the function calculates the SPM concentrations
  in according to Novoa et al., (2017). Default is `TRUE`.

- calc_TUR:

  A `logical`. If `TRUE`, the function calculates the turbidity (FNU) in
  according to Novoa et al., (2017). Default is `TRUE`.

- calc_TSS:

  A `logical`. If `TRUE`, the function calculates the TSS concentrations
  in according to Jiang et al., (2021). Default is `TRUE`.

- calc_gons:

  A `logical`. If `TRUE`, the function calculates chlorophyll using Gons
  et al. (2002) algorithm (NIR ~782 nm). Default is `TRUE`.

- calc_gons740:

  A `logical`. If `TRUE`, the function calculates chlorophyll using Gons
  et al. (2002) algorithm (NIR ~740 nm). Default is `TRUE`.

- calc_NDCI:

  A `logical`. If `TRUE`, the function calculates The Normalised
  Difference Chlorophyll Index algorithm by Mishra and Mishra (2012).
  Default is `TRUE`.

- calc_mishra:

  A `logical`. If `TRUE`, the function calculates chlorophyll using
  Mishra and Mishra (2012) algorithm. Default is `TRUE`.

- calc_dom_wave:

  A `logical`. If `TRUE`, the function calculates the hue angle and the
  dominant wavelength. Default is `TRUE`.

- calc_OWT:

  A `logical`. If `TRUE`, the function calculates the Optical Water Type
  classification in according to Bi and Hieronymi (2024) (OWT_class),
  the membership probability (OWT_score), and the average Z-score
  (OWT_z_dist). In addition, it adds a column with the description of
  the corresponding OWT class. "OWT_class" represents the optical
  category to which the analyzed spectral signature belongs based on
  Gaussian Likelihood. "OWT_score" is a value between 0 and 1
  representing the fuzzy membership grade; it indicates the probability
  of belonging to the selected class relative to the other available
  classes. "OWT_z_dist" indicates the statistical distance between the
  observed spectrum and the class mean, weighted by its standard
  deviation. 0-1.5 indicate an excellent fit with the OWT reference.
  Values above 3 suggest that the spectral signature is an outlier or
  deviates significantly from the typical range of that class. Default
  is `TRUE`.

- save_csv:

  A `logical`. If `TRUE`, the function saves the reflectance data.

- out_dir:

  A `character`. The directory where the CSV file will be saved. Default
  is "outputs" within the working directory.

## Value

A `tibble` with the spectral signatures that have passed QC operation.
In addition, a message containing the reason behind the elimination of
each anomalous spectral signature

## Author

Alessandro Oggioni, phD <alessandro.oggioni@cnr.it>

Nicola Ghirardi, phD <nicola.ghirardi@cnr.it>

## Examples

``` r
# example code
if (FALSE) { # \dontrun{
## Not run:
reflect_data_qc <- wisp_qc_reflectance_data(
  data = reflect_data,
  maxPeak = 0.05,
  maxPeak_blue = 0.02,
  qa_threshold    = 0.5,
  qwip_threshold  = 0.2,
  calc_scatt = TRUE,
  calc_SPM = TRUE,
  calc_TUR = TRUE,
  calc_TSS = TRUE,
  calc_gons = TRUE,
  calc_gons740 = TRUE,
  calc_NDCI = TRUE,
  calc_mishra = TRUE,
  calc_dom_wave = TRUE,
  calc_OWT = TRUE,
  save_csv = FALSE,
  out_dir = "outputs"
)
} # }
## End (Not run)
```
