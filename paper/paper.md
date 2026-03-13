---
title: "WISP.data package"
tags:
  - R
  - WISPstation
  - spectroradiometer
  - remote sensing
  - spectral signatures analysis
authors:
  - name: Nicola Ghirardi
    orcid: 0000-0002-5006-9853
    equal-contrib: true
    affiliation: "1, 2"
  - name: Alessandro Oggioni
    orcid: 0000-0002-7997-219X
    equal-contrib: true
    affiliation: 2
  - name: Mariano Bresciani
    orcid: 0000-0002-7185-8464
    equal-contrib: false
    affiliation: 2
  - name: Gian Marco Scarpa
    orcid: 0000-0002-5061-5607
    equal-contrib: false
    affiliation: 3
  - name: Federica Braga
    orcid: 0000-0002-4131-9080
    equal-contrib: false
    affiliation: 3
affiliations:
  - name: CNR - Institute of Bioeconomy (IBE), Via Madonna del Piano 10, 50019 Firenze, Italy
    index: 1
    ror: 05wq02383
  - name: CNR - Institute of Electromagnetic Sensing of the Environment (IREA), Via A. Corti 12, 20133 Milan, Italy
    index: 2
    ror: 02wxw4x45
  - name: CNR - Institute of Marine Sciences (ISMAR), Arsenale - Tesa 104, Castello 2737/F, 30122 Venice, Italy
    index: 3
    ror: 01t306r88
date: 10 March 2026
bibliography: paper.bib
output: pdf_document
header-includes:
  - \usepackage[labelfont=bf, labelseparator=period, justification=centering]{caption}
---

# Summary

`WISP.data` is an R package designed to automate the acquisition, quality control (QC), analysis, and visualization of spectral data collected by the WISPstation fixed spectroradiometer.
Developed by Water Insight as an evolution of the handheld WISP-3 system, the WISPstation is a fixed instrument that records radiance and irradiance across a wavelength range of 350 to 1100 nm (spectral resolution of 4.6 nm). 
This package serves as a crucial bridge between Water Insight API services and the research community, enabling the transformation of native reflectance measurements into scientifically validated, analysis-ready products through fully reproducible and transparent workflows.

The WISPstation operates with 8 specialized channels to optimize data collection:

* **Dual-directional radiance**: two sets of sensors (NNW and NNE) measure upwelling radiance ($L_u$) and sky radiance ($L_{sky}$) at 40° angles.
* **Smart orientation**: the system automatically selects the best-oriented sensor set based on the Sun's position, maintaining a relative azimuth angle of approximately 135°.
* **Irradiance and calibration**: includes two downwelling irradiance ($E_s$) channels and two unexposed “dark” channels to assess sensor degradation over time.

The primary aim of `WISP.data` is the derivation of Remote Sensing Reflectance (Rrs), defined as the ratio between water-leaving radiance ($L_w$) and downwelling irradiance ($E_s$):

$$\text{Rrs}(\lambda) = \frac{L_w(\lambda)}{E_s(\lambda)} = \frac{L_u(\lambda) - \rho \cdot L_{sky}(\lambda)}{E_s(\lambda)}$$

Where $\rho$ is the Fresnel reflection coefficient.

The package handles the complex transition from total upwelling radiance ($L_u$), which includes unwanted sky-glint and sun-glint, to the pure water-leaving signal ($L_w$) by integrating $L_{sky}$ and $E_s$ measurements into standardized atmospheric correction algorithms.

`WISP.data` provides a modular set of R functions for:

* downloading WISPstation data;
* performing automated and reproducible quality control (QC);
* performing automated skyglint removal (SR);
* applying different algorithms to spectral measurements;
* visualizing spectral and derived data products.

This makes `WISP.data` an ideal solution for operational water quality monitoring, long-term research applications, and integration into large-scale environmental data pipelines.

# Statement of need

The WISPstation is a fixed spectrometer that plays a crucial role in the continuous monitoring of water quality; beyond providing high-frequency spectral measurements, it delivers specialized water quality products derived through various algorithms [@Gons:1997; @Gons:2005; @Simis:2006; @vanderWoerd:2008], essential for environmental observation, ecosystem assessment, and long-term trend analysis. 
However, the effective management and scientific use of these spectral data and derived products present several significant challenges.
Data retrieval through API services is often labor-intensive and technically demanding, especially for users without advanced programming experience. 
In addition, native spectral measurements are sometimes affected by radiometric problems making robust and standardized quality control procedures indispensable. 
Without rigorous filtering and validation protocols, derived products can be unreliable or scientifically misleading.
A further critical barrier lies in the interpretation of spectral signatures themselves. 
For non-expert users, it is difficult to assess the physical and optical plausibility of reflectance spectra, identify anomalous signals, or distinguish between instrument artifacts and real environmental variability. 
Moreover, the application of third-party bio-optical algorithms for the estimation of water quality products typically requires substantial domain knowledge, careful parameterization, and consistent preprocessing workflows, which are rarely standardized across studies.

The scientific validity and operational reliability of WISPstation measurements have been demonstrated in several studies, ranging from the detection of climate-driven chlorophyll-a changes during extreme events [@Free:2021] to the analysis of phytoplankton spatio-temporal dynamics in Lake Trasimeno [@Bresciani:2020]. 
Despite these successful applications, the processing of WISPstation data has been a labor-intensive and time-consuming.
Prior to the development of `WISP.data`, researchers often had to manually inspect individual spectral signatures to identify outliers before the data could be used to estimate water quality parameter. 
This manual quality control process is prone to subjectivity and significantly limits the scalability of high-frequency monitoring. 

`WISP.data` addresses these challenges by providing an integrated, reproducible, and user-oriented software ecosystem that unifies data acquisition, quality control, spectral validation, and product generation within a single R-based framework. 
By lowering technical and methodological barriers, the package enables both expert and non-expert users to transform native WISPstation measurements into reliable, scientifically consistent water quality products, fostering reproducibility, comparability, and broader adoption of spectral monitoring technologies in aquatic research and operational monitoring.

# State of the field                                                        

The current benchmark for processing WISPstation data is WISPcloud, a proprietary web-based infrastructure developed by Water Insight. 
While WISPcloud provides an efficient 'plug-and-play' solution by delivering ready-to-use products, its 'black-box' nature poses significant challenges for rigorous scientific inquiry. 
Specifically, the platform lacks the flexibility to modify critical processing parameters, test third-party algorithms, or customize quality control thresholds.
In the R ecosystem, while established packages exist for general hyperspectral analysis (e.g., `hyperSpec`) or for specific in-water optical profilers (e.g., `Cops`), there is a notable software gap for autonomous, fixed, above-water spectroradiometers. 
`WISP.data` fills this void by providing an end-to-end, open-source pipeline that adheres to FAIR principles.
Rather than merely replicating existing cloud functionalities, `WISP.data` transforms a proprietary workflow into an open, verifiable, and scientifically controllable ecosystem.

# Software design

The design philosophy of `WISP.data` is based on three core principles: (1) a modular, pipeline-oriented workflow abstracting complex API interactions, (2) the adoption of community standards for data manipulation and physical units, and (3) a "transparent-box" approach to quality control, sky-glint removal, and bio-optical modeling.
The complete workflow of the `WISP.data` package is shown in Figure \ref{fig:workflow}.

![*WISP.data workflow.*](Figure_1.png){#fig:workflow width=100%}

The package architecture is organized into functional modules:

* **Data Integration Layer**: the functions `wisp_get_reflectance_data()` and its iterative wrapper `wisp_get_reflectance_multi_data()` manage the communication with the Water Insight API using the `httr2` framework. Raw JSON/text responses are parsed, unnested, and converted into structured `tibble` objects. This layer also handles the transformation of spectral columns into a standardized format, making the data immediately ready for analysis.
* **Physical Awareness**: `WISP.data` integrates the `units` package to maintain physical consistency. Every measurement retrieved is assigned its proper unit, preventing dimensional errors in downstream calculations.
* **Modular Quality Control (QC)**: the `wisp_qc_reflectance_data()` function implements a multi-step diagnostic pipeline, allowing for a sequential execution of six distinct QC tests (QC1–QC6) alongside advanced validation metrics like QA [@Wei:2016] and QWIP [@Dierssen:2022]. The architecture is highly parameterized, enabling researchers to tune thresholds (e.g., `maxPeak`, `maxPeak_blue`, `qa_threshold`, and `qwip_threshold`) to suit specific optical water types, from oligotrophic to highly turbid environments.
* **Sky-glint removal (SR)**: the `wisp_sr_reflectance_data()` function implements a sky-glint removal algorithm based on the methodology proposed by @Jiang:2020.
* **Application of third-party algorithms**: `WISP.data` currently supports a wide array of standardized algorithms for assessing the physical and optical properties of water [@Lee:2002; @vanderWoerd:2015; @vanderWoerd:2018; @Bi:2024] and for estimating the concentrations of water quality parameters [@Gons:1999; @Gons:2002; @Nechad:2010; @Mishra:2012; @Novoa:2017; @Jiang:2021]. These are implemented as modular flags within `wisp_qc_reflectance_data()` and `wisp_sr_reflectance_data()` functions, allowing for a seamless "one-call" workflow from native spectra to validated products.
* **Visualization layer**: utilizing `ggplot2` and `plotly` as backends, the package provides consistent visualization interfaces. Functions like `wisp_plot_reflectance_data()`, `wisp_plot_comparison()` (Figure \ref{fig:spectra}), and `wisp_trend_plot()` (Figures \ref{fig:trendsingle}, \ref{fig:trendmulti}) are designed to handle both publication-quality plots and interactive exploration.

![*Plot resulting from the `wisp_plot_comparison()` function showing the comparison between: A) native WISPstation Rrs, B) Rrs filtered by “QC”, C) Rrs to which “SR” has been applied (site: Trasimeno; period: 11/09/2024 – 17/09/2024).*](Figure_2.png){#fig:spectra width=100%}

![*Plot resulting from the `wisp_trend_plot()` function showing the temporal trend of three exemplary parameters (from top to bottom: "Novoa_SPM", "Novoa_TUR", and "Mishra_CHL") for 25/07/2024 from 8 a.m. to 4 p.m. (site: Trasimeno).*](Figure_3.png){#fig:trendsingle width=100%}

![*Plot resulting from the `wisp_trend_plot()` function showing the temporal trend of five exemplary parameters averaged on a daily basis: in the upper plot, a comparison between the WISPstation native algorithm (TSM) and two third-party algorithms for estimating suspended solids concentration ("Novoa_SPM" and "Jiang_TSS"); in the lower plot, the comparison between the WISPstation native chlorophyll-a algorithm (Chla) and Mishra_CHL algorithm. (site: Trasimeno; period: 01/05/2024 – 10/05/2024).*](Figure_4.png){#fig:trendmulti width=100%}

# Research impact statement (---) 

To date, the `WISP.data` package has been successfully deployed and tested at two distinct WISPstation sites: Lake Trasimeno (43.122° N, 12.134° E) and the Po River (ISMAR).
Lake Trasimeno, located in central Italy, is the fourth-largest lake in Italy (124 km^2^). 
The basin is characterized by intensive agricultural land use and livestock farming, which lead to significant nutrient loading. 
These high nutrient inputs frequently trigger phytoplankton blooms, including potentially harmful cyanobacteria. 
The lake's complex hydrological conditions, combined with climatic drivers such as temperature and wind-induced resuspension, result in high intra-day and inter-day variability. 
In such dynamic environments, using a fixed spectroradiometer such as the WISPstation is essential for capturing rapid shifts and understanding water quality trends at different time scales.

Regarding Po river ... (ISMAR)

The first WISPstation was installed in April 2018 on Lake Trasimeno. 
A second deployment took place in September 2024 along the Po River, as part of the ITINERIS project (PNRR) within the Italian DANUBIUS-RI Supersite. 
Both areas – Lake Trasimeno (https://deims.org/c93c97fb-196e-480a-87b0-817218cd7c24) and the Po Delta (https://deims.org/6869436a-80f4-4c6d-954b-a730b348d7ce) – are recognized as research sites of the European eLTER-RI Infrastructure. 
Since their installation, the instruments have operated continuously, acquiring hyperspectral above-water radiometric data (350–900 nm) every 15 minutes during daylight hours, capturing a wide range of optical water conditions.

The scientific utility of `WISP.data` was recently demonstrated in a study by Ghirardi et al. (2026, Under Review). 
This research investigates the spatio-temporal dynamics of Suspended Particulate Matter (SPM) and Chlorophyll-a (CHL) in Lake Trasimeno using a synergistic approach. 
By combining high-frequency in situ data processed via `WISP.data` with multisensor satellite imagery (2019–2024), the study validated Rrs products from 14 different satellite sensors. 
The strong agreement between the satellite-derived data and the `WISP.data` processed ground-truth enabled the accurate retrieval of SPM and CHL concentrations. 
These results highlight the package's role in bridging the gap between local autonomous measurements and large-scale satellite monitoring, facilitating the study of rapid environmental changes in lake ecosystems.

For the Po River, the package was employed to ... (ISMAR)

# AI usage disclosure

No generative AI tools were used in the the writing of this manuscript, or the preparation of supporting materials.
Generative AI tools were used as a support for code development.

# Acknowledgements (---) 

This research is funded by EU - Next Generation EU Mission 4, Component 2 - CUP B53C22002150006 - Project IR0000032 – ITINERIS - Italian Integrated Environmental Research Infrastructures System

# References