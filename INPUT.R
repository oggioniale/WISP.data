source("C:/Users/NicolaG/Desktop/R/WISP.data/R/functions.R")

### Download data --------------------------------------------------------------
reflect_data <- wisp_get_reflectance_multi_data(
  time_from = "2026-04-29T06:00",
  time_to   = "2026-04-30T23:00",
  station   = "WISPstation012",
  userid    = "cnr_irea",
  pwd       = "W1spcloud4cnr_irea",
  save_csv  = FALSE,
  out_dir   = "C:/Users/NicolaG/Downloads"
)
### ----------------------------------------------------------------------------


### Quality Control (QC) -------------------------------------------------------
reflect_data_qc <- wisp_qc_reflectance_data(
  data           = reflect_data,
  maxPeak        = 0.05,
  maxPeak_blue   = 0.02,
  qa_threshold   = 0.5,
  qwip_threshold = 0.2,
  calc_scatt     = TRUE,
  calc_SPM       = TRUE,
  calc_TUR       = TRUE,
  calc_TSS       = TRUE,
  calc_gons      = TRUE,
  calc_gons740   = TRUE,
  calc_NDCI      = TRUE,
  calc_mishra    = TRUE,
  calc_dom_wave  = TRUE,
  calc_OWT       = TRUE,
  save_csv       = FALSE,
  out_dir        = "C:/Users/NicolaG/Downloads"
)
### ----------------------------------------------------------------------------


### Sunglint removal (Jiang et al., 2020) (SR) ---------------------------------
reflect_data_sr <- wisp_sr_reflectance_data(
  qc_data       = reflect_data_qc,
  calc_scatt    = TRUE,
  calc_SPM      = TRUE,
  calc_TUR      = TRUE,
  calc_TSS      = TRUE,
  calc_gons     = TRUE,
  calc_gons740  = TRUE,
  calc_NDCI     = TRUE,
  calc_mishra   = TRUE,
  calc_dom_wave = TRUE,
  calc_OWT      = TRUE,
  save_csv      = FALSE,
  out_dir       = "C:/Users/NicolaG/Downloads"
)
### ----------------------------------------------------------------------------


### Plot (spectral signatures) -------------------------------------------------
custom_raw <- list(
  legend_TSM  = TRUE, 
  legend_Chla = TRUE, 
  legend_Kd   = TRUE, 
  legend_cpc  = TRUE
)

custom_qc <- list(
  legend_TSM            = TRUE, 
  legend_Chla           = TRUE, 
  legend_Kd             = TRUE, 
  legend_cpc            = TRUE, 
  legend_scatt          = TRUE,
  legend_ratio          = TRUE,
  legend_novoa_SPM      = TRUE,
  legend_novoa_TUR      = TRUE,
  legend_jiang_TSS      = TRUE,
  legend_gons_CHL       = TRUE,
  legend_gons740_CHL    = TRUE,
  legend_NDCI           = TRUE,
  legend_mishra_CHL     = TRUE,
  legend_hue_angle      = TRUE, 
  legend_dom_wavelength = TRUE,
  legend_OWT_class      = TRUE,
  legend_OWT_score      = TRUE,
  legend_OWT_z_dist     = TRUE
)

custom_sr <- list(
  legend_TSM            = TRUE, 
  legend_Chla           = TRUE, 
  legend_Kd             = TRUE, 
  legend_cpc            = TRUE, 
  legend_scatt          = TRUE,
  legend_ratio          = TRUE,
  legend_novoa_SPM      = TRUE,
  legend_novoa_TUR      = TRUE,
  legend_jiang_TSS      = TRUE,
  legend_gons_CHL       = TRUE,
  legend_gons740_CHL    = TRUE,
  legend_NDCI           = TRUE,
  legend_mishra_CHL     = TRUE,
  legend_hue_angle      = TRUE, 
  legend_dom_wavelength = TRUE,
  legend_OWT_class      = TRUE,
  legend_OWT_score      = TRUE,
  legend_OWT_z_dist     = TRUE
)

fig_comparison <- wisp_plot_comparison(
  raw_data = reflect_data, 
  qc_data  = reflect_data_qc, 
  sr_data  = reflect_data_sr,
  raw_args = custom_raw,
  qc_args  = custom_qc,
  sr_args  = custom_sr
)
print(fig_comparison)
### ----------------------------------------------------------------------------


### Plot (temporal trend) ------------------------------------------------------
fig_trend <- wisp_trend_plot(
  data       = reflect_data_sr,
  params     = c("TSM", "Chla", "Novoa_SPM", "Mishra_CHL", "Hue_Angle", "Dom_Wave"),
  aggregate  = "none",
  merge_plot = TRUE
)
print(fig_trend)
### ----------------------------------------------------------------------------
