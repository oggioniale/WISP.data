---
title: "WISP.data: An open-source R package for processing spectral data from above-water autonomous spectroradiometers"
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
  - name: Gian Marco Scarpa
    orcid: 0000-0002-5061-5607
    equal-contrib: false
    affiliation: "3"
  - name: Dalin Jiang
    orcid: 0000-0001-5676-5860
    equal-contrib: false
    affiliation: "4"
  - name: Federica Braga
    orcid: 0000-0002-4131-9080
    equal-contrib: false
    affiliation: "3"
  - name: Mariano Bresciani
    orcid: 0000-0002-7185-8464
    equal-contrib: false
    affiliation: "2"
  - nome: Water Insight(??)
    orcid: 0000-0000-0000-0000
    equal-contrib: false
    affiliation: "5"
  - name: Alessandro Oggioni
    orcid: 0000-0002-7997-219X
    equal-contrib: true
    affiliation: "2"
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
  - name: University of Stirling - Earth and Planetary Observation Sciences (EPOS), Biological and Environmental Sciences, Faculty of Natural Sciences, Stirling FK9 4LA, UK
    index: 4
    ror: 04m01e232
  - name: Water Insight - address
    index: 5
date: 20 April 2026
bibliography: paper.bib
output: pdf_document
header-includes:
  - \usepackage[labelfont=bf, labelseparator=period, justification=centering]{caption}
---

# Summary

`WISP.data` is an R package designed to automate the download, quality control (QC), processing, analysis, and visualization of spectral data collected by WISPstation fixed spectroradiometers.
Developed by Water Insight B.V. (Ede, The Netherlands) as an evolution of the handheld WISP-3 system [@Hommersom:2012], the WISPstation is an autonomous above water instrument installed at fixed position that records radiance and irradiance across a wavelength range of 350 to 900 nm, with a spectral resolution of 4.6 nm at high frequency (e.g. every 15 minutes, during daytime).
This R package serves as a crucial bridge between Water Insight API services and the scientific users, enabling the conversion of native reflectance measurements into robust, analysis-ready products through fully reproducible and transparent workflows, from an Open Science perspective.

The package performs the retrieval of water-leaving radiance ($L_w$) from total upwelling radiance ($L_u$), which includes sky- and sun-glint contributions, by integrating $L_{sky}$ and $E_s$ measurements.
`WISP.data` provides a modular set of R functions for: I. downloading data from specific WISPstation by making use of the solutions provided by Water Insight itself (APIs and now via user accounts); II. performing automated and reproducible quality control (QC); III. performing automated sky-glint removal (SR); IV. applying empirical and semi‑analytical bio‑optical algorithms to spectral measurements to retrieve optical properties and water quality parameters; V. visualizing spectral and derived data products.

This makes `WISP.data` an ideal solution for satellite-derived product validation, operational water quality monitoring, long-term research applications, and integration into large-scale environmental data pipelines.

# Statement of need

The WISPstation is an autonomous above water fixed spectrometer installed at fixed position, that supports continuous monitoring of water quality; beyond providing high-frequency spectral measurements, it provides water quality products derived from selected algorithms [@Gons:1997; @Gons:2005; @Simis:2006; @vanderWoerd:2008], essential for environmental observation, ecosystem assessment, and long-term trend analysis.

The WISPstation operates with 8 specialized channels to optimize data collection:

* **Dual-directional radiance**: two sets of sensors (looking NNW and NNE) measure upwelling radiance ($L_u$) and sky radiance ($L_{sky}$) at 40° angles.
* **Irradiance and calibration**: includes two downwelling irradiance ($E_s$) channels and two unexposed “dark” channels to assess sensor degradation over time.

The system automatically selects the best-oriented sensor set based on the Sun's position, maintaining a relative azimuth angle of approximately 135°.

The primary aim of `WISP.data` is the derivation of Remote Sensing Reflectance (R~rs~), defined as the ratio between water-leaving radiance ($L_w$) and downwelling irradiance ($E_s$):

$$\text{R}_{rs}(\lambda) = \frac{L_w(\lambda)}{E_s(\lambda)} = \frac{L_u(\lambda) - \rho \cdot L_{sky}(\lambda)}{E_s(\lambda)}$$

Where $\rho$ is the Fresnel reflection coefficient, $L_u$ is the total upwelling radiance and $L_{sky}$ is the sky radiance contributing to surface reflections.

However, the effective management and scientific use of these spectral data and derived products present several significant challenges.
Data retrieval through API services is often labor-intensive and technically demanding, especially for users without advanced programming experience.
In addition, native spectral measurements could be affected by radiometric issues making the implementation of consistent and rigorous quality control procedures essential.
Without rigorous filtering and validation protocols, derived products can be unreliable or scientifically misleading.
A further critical barrier lies in the interpretation of spectral signatures themselves.
For non-expert users, it is difficult to assess the physical and optical reliability of reflectance spectra, identify anomalous signals, or distinguish between instrument geometry artifacts and real environmental variability.
Moreover, the application of third-party bio-optical algorithms for the estimation of water quality parameters typically requires substantial domain knowledge, careful parameterization, and consistent preprocessing workflows, which are rarely standardized across studies.

The scientific validity and operational reliability of WISPstation measurements have been demonstrated in several studies, ranging from the detection of climate-driven chlorophyll-a changes during extreme events [@Free:2021] to the analysis of phytoplankton spatio-temporal dynamics in Lake Trasimeno [@Bresciani:2020].
Despite these successful applications, the processing of WISPstation data has been labor-intensive and time-consuming.
Prior to the development of `WISP.data`, researchers often had to manually inspect individual spectral signatures to identify outliers before the data could be used to estimate water quality parameter.
This manual quality control process is prone to subjectivity and significantly limits the scalability of high-frequency monitoring.

`WISP.data` addresses these challenges by delivering an integrated, transparent, reproducible, and user-oriented software ecosystem that unifies data acquisition, quality control, and product generation within a single R-based framework.
By lowering technical and methodological barriers, the package enables both expert and non-expert users to transform native WISPstation measurements into reliable, scientifically consistent water quality products, fostering reproducibility, comparability, and broader adoption of spectral monitoring technologies in aquatic research and operational monitoring.

# State of the field                            

The current benchmark for processing WISPstation data is WISPcloud (https://wispcloud.waterinsight.nl/), a proprietary web-based infrastructure developed by Water Insight.
While WISPcloud provides an efficient 'plug-and-play' solution by delivering ready-to-use products, its 'black-box' nature poses significant challenges for rigorous scientific inquiry.
Specifically, the platform lacks the flexibility to modify critical processing parameters, test third-party algorithms, or customize site-specific quality control thresholds.

In the R ecosystem, while established packages exist for general hyperspectral analysis (e.g., `hyperSpec` [@hyperSpec]) or for specific in-water optical profilers (e.g., `cops` [@cops; @cops-paper]), there is a notable software gap for autonomous, fixed, above-water spectroradiometers.
Rather than merely replicating existing cloud functionalities, `WISP.data` transforms a proprietary workflow into an open, verifiable, and scientifically controllable ecosystem.

# Software design

The design philosophy of `WISP.data` is based on three characteristics: I. a modular, pipeline-oriented workflow abstracting complex API interactions; II. the adoption of community standards for data manipulation and physical units; III. a "transparent-box" approach to quality control, sky-glint removal, and bio-optical algorithms application.
The complete workflow of the `WISP.data` package is shown in Figure \ref{fig:workflow}.

![*WISP.data workflow.*](Figure_1.png){#fig:workflow width=100%}

The package architecture is organized into functional modules:

* **Data Integration Layer**: the functions `wisp_get_reflectance_data()` and its iterative wrapper `wisp_get_reflectance_multi_data()` manage the communication with the Water Insight API using the `httr2` framework. These functions allow users to filter measurements by specific dates and time periods. Raw JSON/text responses are parsed, unnested, and converted into structured `tibble` objects. This layer also handles the transformation of spectral columns into a standardized format, making the data immediately ready for analysis.
* **Physical Awareness**: `WISP.data` integrates the `units` package to maintain physical consistency. Every measurement retrieved is assigned its proper unit, preventing dimensional errors in downstream calculations.
* **Modular Quality Control (QC)**: the `wisp_qc_reflectance_data()` function implements a multi-step diagnostic pipeline, allowing for a sequential execution of six distinct QC tests (QC1–QC6) alongside advanced validation metrics like Quality Assurance (QA) [@Wei:2016] and Quality Water Index Polynomial (QWIP) [@Dierssen:2022]. The architecture is highly parameterized, enabling researchers to tune thresholds (e.g., `maxPeak`, `maxPeak_blue`, `qa_threshold`, and `qwip_threshold`) to suit specific optical water types, from oligotrophic to highly turbid and productive environments.
* **Sky-glint removal (SR)**: the `wisp_sr_reflectance_data()` function implements a sky-glint removal algorithm based on the methodology proposed by @Jiang:2020.
* **Application of third-party algorithms**: `WISP.data` currently supports a wide array of standardized algorithms for assessing the physical and optical properties of water [@Lee:2002; @vanderWoerd:2015; @vanderWoerd:2018; @Bi:2024] and for estimating the concentrations of water quality parameters [@Gons:1999; @Gons:2002; @Nechad:2010; @Mishra:2012; @Novoa:2017; @Jiang:2021]. These are implemented as modular flags within `wisp_qc_reflectance_data()` and `wisp_sr_reflectance_data()` functions, allowing for a seamless "one-call" workflow from native spectra to validated products.
* **Visualization layer**: utilizing `ggplot2` and `plotly` as backends, the package provides consistent visualization interfaces. Functions like `wisp_plot_reflectance_data()`, `wisp_plot_comparison()` (Figure \ref{fig:spectra}), and `wisp_trend_plot()` (Figures \ref{fig:trendsingle}, \ref{fig:trendmulti}) are designed to handle both publication-quality plots and interactive exploration.

![*Plot resulting from the `wisp_plot_comparison()` function showing the comparison between: A) native WISPstation R~rs~, B) R~rs~ filtered by “QC”, C) R~rs~ to which “SR” has been applied (site: Trasimeno; period: 11/09/2024 – 17/09/2024).*](Figure_2.png){#fig:spectra width=100%}

![*Plot resulting from the `wisp_trend_plot()` function showing the temporal trend of three exemplary parameters (from top to bottom: "Novoa_SPM", "Novoa_TUR", and "Mishra_CHL") for 25/07/2024 from 8 a.m. to 4 p.m. (site: Trasimeno).*](Figure_3.png){#fig:trendsingle width=100%}

![*Plot resulting from the `wisp_trend_plot()` function showing the temporal trend of five exemplary parameters averaged on a daily basis: in the upper plot, a comparison between the WISPstation native algorithm (TSM) and two third-party algorithms for estimating suspended solids concentration ("Novoa_SPM" and "Jiang_TSS"); in the lower plot, the comparison between the WISPstation native chlorophyll-a algorithm (Chla) and Mishra_CHL algorithm. (site: Trasimeno; period: 01/05/2024 – 10/05/2024).*](Figure_4.png){#fig:trendmulti width=100%}

# Research impact statement

The package was successfully tested using two WISPstations installed in two different aquatic environments in Italy.
The first was installed in April 2018 in Lake Trasimeno, on a lake platform located approximately 400 m from Polvese Island (43.122° N, 12.134° E).
The second was deployed in September 2024 along the Po River at Pontelagoscuro (44.889° N, 11.6106° E), approximately 95 km upstream from its confluence with the northern Adriatic Sea, on a floating monitoring platform operated by the Agenzia Interregionale per il fiume Po (AIPO).

Lake Trasimeno, located in central Italy, is the fourth-largest lake in the country (124 km^2^).
Its catchment is characterized by intensive agricultural practices and livestock farming, which lead to significant nutrient loading.
These conditions frequently promote phytoplankton blooms, including potentially harmful cyanobacteria. The lake exhibits complex hydrodynamic behavior, with strong intra-day and inter-day variability driven by meteorological forcing such as temperature fluctuations and wind-induced sediment resuspension.
The Po River, the longest river in Italy, flows across the Po Valley and governs the exchanges at the river–sea interface, driving the transfer of sediments, nutrients, and organic matter toward the northern Adriatic Sea and influencing coastal biogeochemistry and water quality.
The river is characterized by highly dynamic hydrological conditions, with discharge variability driven by seasonal cycles as well as extreme events such as floods and prolonged drought periods.
These conditions strongly affect sediment transport and turbidity, resulting in rapid changes in water optical properties.

Both environments are the focus of long-standing research efforts aimed at understanding the spatio-temporal dynamics of parameters that can also be monitored through remote sensing.
The stations are officially designated as research sites within European Strategy Forum on Research Infrastructures (ESFRI).
Particularly, Lake Trasimeno[^1] is part of the Integrated European Long-Term Ecosystem, Critical Zone and Socio-Ecological Research (eLTER RI)[^2], while the Po River is a Supersite of DANUBIUS-RI[^3], the International Centre for Advanced Studies on River-Sea Systems, a pan-European distributed Research Infrastructure enabling integrated studies of rivers and their catchments, transitional waters such as estuaries, deltas and lagoons, and their adjacent coastal seas.

[^1]: https://deims.org/c93c97fb-196e-480a-87b0-817218cd7c24
[^2]: https://elter-ri.eu
[^3]: https://danubius-ri.eu

Within these RIs, the availability of continuous, comparable, and quality-controlled measurements is essential to minimize uncertainties and ensure data reliability.
Such measurements, acquired through high-quality sensor systems and complemented by parameters derived from robust algorithms, can be particularly valuable for the calibration and validation (Cal/Val) of satellite-derived products.
Within eLTER RI, these measurements contribute to the definition of standardized variables [@Zacharias:2026], known as Standard Observations (SOs), which integrate target variables, measurement methods, and protocols to ensure harmonized data collection across sites at the continental scale.
Among these, the SO “Phenological traits – SOBIO_15” specifically addresses the use of remote sensing approaches for monitoring vegetation phenological dynamics, including in aquatic ecosystems, encompassing both inland standing waters (lakes) and running waters (rivers).

The scientific utility of `WISP.data` has recently been demonstrated by Ghirardi et al. (under review), who investigated the spatio-temporal dynamics of suspended particulate matter and chlorophyll-a in Lake Trasimeno using a synergistic approach.
By combining high-frequency in situ observations processed with `WISP.data` and multisensor satellite imagery (2019–2024), the study validated R~rs~ products from 14 different satellite sensors.
The strong agreement between satellite-derived estimates and the `WISP.data` processed ground-truth enabled the accurate retrieval of both water quality parameters.
These results highlight the package's role in bridging the gap between local autonomous measurements and large-scale satellite monitoring, facilitating the study of rapid environmental changes in lake ecosystems.
For the Po River, `WISP.data` is used to process high-frequency hyperspectral radiometric data collected at the Pontelagoscuro platform, including quality control, sky-glint correction, and the application of algorithms to derive turbidity from optical measurements.
The resulting time series of above-water observations supports the assessment of hyperspectral and multispectral satellite missions, enabling the testing of different atmospheric correction algorithms.
In addition, water turbidity derived from WISPstation measurements is compared with in situ data acquired by in-water turbidimeters, enabling consistency evaluation between radiometric-derived and direct observations.
This integrated approach enhances the interpretation of turbidity dynamics in highly variable riverine systems.

Looking ahead, the modular architecture of `WISP.data` is well-suited to be extended and adapted to other optical view fixed spectroradiometric systems widely used in the scientific community for water body monitoring, thereby promoting greater interoperability and standardization in the processing of in situ optical data.

# AI usage disclosure

No generative AI tools were used in the writing of this manuscript, or the preparation of supporting materials.
Generative AI tools were used as a support for code development.

# Acknowledgements

The development of this package and the related publication has been funded by the European Union – Next Generation EU, Mission 4, Component 2 – CUP B53C22002150006, within the Project IR0000032 – ITINERIS (Italian Integrated Environmental Research Infrastructures System), and has been partially supported by the European Union’s Horizon 2020 research and innovation programme under the H2020 eLTER‑Plus project (Grant Agreement No. 871128, DOI: 10.3030/871128) and the eLTER EnRich project (Grant Agreement No. 101131751, DOI: 10.3030/101131751).

# References