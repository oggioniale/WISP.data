# **WISP.data**

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16893167.svg)](https://doi.org/10.5281/zenodo.16893167)

# Summary

`WISP.data` is an R package designed to automate the acquisition,
quality control (QC), analysis, and visualization of spectral data
collected by the WISPstation fixed spectroradiometer. Developed by Water
Insight as an evolution of the handheld WISP-3 system, the WISPstation
is a fixed instrument that records radiance and irradiance across a
wavelength range of 350 to 1100 nm (spectral resolution of 4.6 nm). This
package serves as a crucial bridge between Water Insight API services
and the research community, enabling the transformation of raw
reflectance measurements into scientifically validated, analysis-ready
products through fully reproducible and transparent workflows.

The WISPstation operates with 8 specialized channels to optimize data
collection:

- **Dual-directional radiance**: two sets of sensors (NNW and NNE)
  measure upwelling radiance ($`L_u`$) and sky radiance ($`L_{sky}`$) at
  40° angles.
- **Smart orientation**: the system automatically selects the
  best-oriented sensor set based on the Sun’s position, maintaining a
  relative azimuth angle of approximately 135°.
- **Irradiance and calibration**: includes two downwelling irradiance
  ($`E_s`$) channels and two unexposed “dark” channels to assess sensor
  degradation over time.

The primary aim of `WISP.data` is the derivation of Remote Sensing
Reflectance (Rrs), defined as the ratio between water-leaving radiance
($`L_w`$) and downwelling irradiance ($`E_s`$):

``` math
\text{Rrs}(\lambda) = \frac{L_w(\lambda)}{E_s(\lambda)} = \frac{L_u(\lambda) - \rho \cdot L_{sky}(\lambda)}{E_s(\lambda)}
```

The package handles the complex transition from total upwelling radiance
($`L_u`$), which includes unwanted sky-glint and sun-glint, to the pure
water-leaving signal ($`L_w`$) by integrating $`L_{sky}`$ and $`E_s`$
measurements into standardized atmospheric correction algorithms.

`WISP.data` provides a modular set of R functions for:

- downloading WISPstation data;
- performing automated and reproducible quality control (QC);
- performing automated skyglint removal (SR);
- applying different algorithms to spectral measurements;
- visualizing spectral and derived data products.

This makes `WISP.data` an ideal solution for operational water quality
monitoring, long-term research applications, and integration into
large-scale environmental data pipelines.

# Statement of need

The WISPstation is a fixed spectrometer that plays a crucial role in the
continuous monitoring of water quality, providing high-frequency
spectral measurements essential for environmental observation, ecosystem
assessment, and long-term trend analysis. However, the effective
management and scientific use of these spectral data present several
significant challenges. Data retrieval through API services is often
labor-intensive and technically demanding, especially for users without
advanced programming experience. In addition, raw spectral measurements
are sometimes affected by radiometric problems making robust and
standardized quality control procedures indispensable. Without rigorous
filtering and validation protocols, derived products can be unreliable
or scientifically misleading. A further critical barrier lies in the
interpretation of spectral signatures themselves. For non-expert users,
it is difficult to assess the physical and optical plausibility of
reflectance spectra, identify anomalous signals, or distinguish between
instrument artifacts and real environmental variability. Moreover, the
application of third-party bio-optical algorithms for the estimation of
water quality products typically requires substantial domain knowledge,
careful parameterization, and consistent preprocessing workflows, which
are rarely standardized across studies.

`WISP.data` addresses these challenges by providing an integrated,
reproducible, and user-oriented software ecosystem that unifies data
acquisition, quality control, spectral validation, and product
generation within a single R-based framework. By lowering technical and
methodological barriers, the package enables both expert and non-expert
users to transform raw WISPstation measurements into reliable,
scientifically consistent water quality products, fostering
reproducibility, comparability, and broader adoption of spectral
monitoring technologies in aquatic research and operational monitoring.

# Installation

You can install the development version of WISP.data directly from
GitHub.  
The following commands will automatically install all required
dependencies:

``` r
# Install remotes if not already available
if (!require("remotes")) install.packages("remotes")

# Install WISP.data and dependencies
remotes::install_github("oggioniale/WISP.data", dependencies = TRUE)
```

# Functions

For a detailed description of each function, please visit this link:
<https://oggioniale.github.io/WISP.data/reference/index.html>

# Quick start

To view a complete example of WISP.data, please visit this page:
<https://oggioniale.github.io/WISP.data/articles/exampleOfFunctions.html>

# WISP_runApp

To view a complete example of WISP_runApp, please visit this page
<https://oggioniale.github.io/WISP.data/reference/wisp_runApp.html>

# Data flow

# Gallery

# References
