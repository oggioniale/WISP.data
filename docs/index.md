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

![Figure 1. WISP.data workflow.](reference/figures/Figure%201.png)

***Figure 1.*** *WISP.data workflow.*

# Gallery

In this section, we showcase some of the typical outputs generated by
`WISP.data`.

![Figure 2. Plot resulting from the ‘wisp_plot_comparison’ function
showing the comparison between “raw” spectral signatures, those filtered
by “QC”, and those to which “SR” has been applied (period: 11/09/2024 –
17/09/2024).](reference/figures/Figure%202.png)

***Figure 2.*** *Plot resulting from the ‘wisp_plot_comparison’ function
showing the comparison between “raw” spectral signatures, those filtered
by “QC”, and those to which “SR” has been applied (period: 11/09/2024 –
17/09/2024).*

![Figure 3. Plot resulting from the ‘wisp_trend_plot’ function showing
the temporal trend of four exemplary parameters (Chla, Mishra_CHL,
Novoa_SPM, and TSM) for 25/07/2024.](reference/figures/Figure%203.png)

***Figure 3.*** *Plot resulting from the ‘wisp_trend_plot’ function
showing the temporal trend of four exemplary parameters (Chla,
Mishra_CHL, Novoa_SPM, and TSM) for 25/07/2024.*

![Figure 4. Plot resulting from the ‘wisp_trend_plot’ function showing
the temporal trend of four exemplary parameters (Chla, Mishra_CHL,
Novoa_SPM, and TSM) averaged on a daily basis (period: 11/09/2024 –
17/09/2024).](reference/figures/Figure%204.png)

***Figure 4.*** *Plot resulting from the ‘wisp_trend_plot’ function
showing the temporal trend of four exemplary parameters (Chla,
Mishra_CHL, Novoa_SPM, and TSM) averaged on a daily basis (period:
11/09/2024 – 17/09/2024).*

# References

- Bi, S., & Hieronymi, M. (2024). Holistic optical water type
  classification for ocean, coastal, and inland waters. *Limnology and
  Oceanography*, 69(7), 1547-1561. <https://doi.org/10.1002/lno.12606>
- Dierssen, H. M., Vandermeulen, R. A., Barnes, B. B., Castagna, A.,
  Knaeps, E., & Vanhellemont, Q. (2022). QWIP: A quantitative metric for
  quality control of aquatic reflectance spectral shape using the
  apparent visible wavelength. *Frontiers in Remote Sensing*, 3, 869611.
  <https://doi.org/10.3389/frsen.2022.869611>.
- Gons, H. J. (1999). Optical teledetection of chlorophyll a in turbid
  inland waters. *Environmental science & technology*, 33(7), 1127-1132.
  <https://doi.org/10.1021/es9809657>.
- Gons, H. J., Rijkeboer, M., & Ruddick, K. G. (2002). A
  chlorophyll-retrieval algorithm for satellite imagery (Medium
  Resolution Imaging Spectrometer) of inland and coastal waters.
  *Journal of Plankton Research*, 24(9), 947-951.
  <https://doi.org/10.1093/plankt/24.9.947>.
- Jiang, D., Matsushita, B., & Yang, W. (2020). A simple and effective
  method for removing residual reflected skylight in above-water remote
  sensing reflectance measurements. *ISPRS Journal of Photogrammetry and
  Remote Sensing*, 165, 16-27.
  <https://doi.org/10.1016/j.isprsjprs.2020.05.003>.
- Jiang, D., Matsushita, B., Pahlevan, N., Gurlin, D., Lehmann, M. K.,
  Fichot, C. G., … & O’Donnell, D. (2021). Remotely estimating total
  suspended solids concentration in clear to extremely turbid waters
  using a novel semi-analytical method. *Remote Sensing of Environment*,
  258, 112386. <https://doi.org/10.1016/j.rse.2021.112386>.
- Lee, Z., Carder, K. L., & Arnone, R. A. (2002). Deriving inherent
  optical properties from water color: a multiband quasi-analytical
  algorithm for optically deep waters. *Applied optics*, 41(27),
  5755-5772. <https://doi.org/10.1364/AO.41.005755>.
- Mishra, S., & Mishra, D. R. (2012). Normalized difference chlorophyll
  index: A novel model for remote estimation of chlorophyll-a
  concentration in turbid productive waters. *Remote Sensing of
  Environment*, 117, 394-406.
  <https://doi.org/10.1016/j.rse.2011.10.016>.
- Nechad, B., Ruddick, K. G., & Park, Y. (2010). Calibration and
  validation of a generic multisensor algorithm for mapping of total
  suspended matter in turbid waters. *Remote Sensing of Environment*,
  114(4), 854-866. <https://doi.org/10.1016/j.rse.2009.11.022>.
- Novoa, S., Doxaran, D., Ody, A., Vanhellemont, Q., Lafon, V., Lubac,
  B., & Gernez, P. (2017). Atmospheric corrections and multi-conditional
  algorithm for multi-sensor remote sensing of suspended particulate
  matter in low-to-high turbidity levels coastal waters. *Remote
  Sensing*, 9(1), 61. <https://doi.org/10.3390/rs9010061>.
- van der Woerd, H. J., & Wernand, M. R. (2015). True colour
  classification of natural waters with medium-spectral resolution
  satellites: SeaWiFS, MODIS, MERIS and OLCI. *Sensors*, 15(10),
  25663-25680. <https://doi.org/10.3390/s151025663>.
- Wei, J., Lee, Z., & Shang, S. (2016). A system to measure the data
  quality of spectral remote‐sensing reflectance of aquatic
  environments. *Journal of Geophysical Research: Oceans*, 121(11),
  8189-8207. <https://doi.org/10.1002/2016JC012126>.
