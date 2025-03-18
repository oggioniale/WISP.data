#' Get data of reflectance (level2) from WISPstation for a specific date
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the data of reflectance from WISPstation for a
#' specific date.
#' @param version A `character`. It is the version of the API data. Default
#' is "1.0"
#' @param time_from A `character`. It is the date and time from which the data
#' is requested.
#' @param time_to A `character`. It is the date and time to which the data
#' is requested.
#' @param station A `character`.  It is the name of the station.
#' @param userid A `character`. It is the userid to access to the data service.
#' @param pwd A `character`. It is the password to access to the data service.
#' @return A `tibble` with measurement id, measurement date, instrument name,
#' level2_quality, set of sensor (irradiance and radiances),
#' waterquality values of TSM, Chla, Kd, and cpc as provided by instrument by default,
#' all the values of reflectance for each wevelength from 350 to 900 nm.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom httr2 request req_url_query req_auth_basic req_perform resp_body_string
#' @importFrom tibble as_tibble
#' @importFrom dplyr slice mutate across rename_with
#' @importFrom tidyr unnest_wider
#' @importFrom lubridate as_datetime
#' @importFrom units set_units
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' # NA data
#' reflect_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2024-09-01T09:00",
#'   time_to = "2024-09-01T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#' 
#' # with data
#' reflect_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2024-08-01T09:00",
#'   time_to = "2024-08-01T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#'
#' # no data for the station selected
#' reflect_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2019-06-20T09:00",
#'   time_to = "2019-06-20T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#' 
#' # The two dates are not consistent
#' reflect_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2019-06-20T09:00",
#'   time_to = "2020-06-20T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#' }
#' ## End (Not run)
#' 
### wisp_get_reflectance_data
wisp_get_reflectance_data <- function(
    version = "1.0",
    time_from = NULL,
    time_to = NULL,
    station = NULL,
    userid = NULL,
    pwd = NULL
) {
  # check if the date is different
  start_date <- substr(time_from, start = 0, stop = 10)
  end_date <-  substr(time_to, start = 0, stop = 10)
  if (start_date == end_date) {
    # data request
    response <- httr2::request("https://wispcloud.waterinsight.nl/api/query") |> 
      httr2::req_url_query(
        SERVICE = "data",
        VERSION = version,
        REQUEST = "GetData",
        INSTRUMENT = station,
        TIME = paste(time_from, time_to, sep = ","),
        INCLUDE = "measurement.id,measurement.date,instrument.name,level2.quality,ed.selected,lu.selected,ld.selected,waterquality.tsm,waterquality.chla,waterquality.kd,waterquality.cpc,level2.reflectance"
      ) |> 
      httr2::req_auth_basic(userid, pwd) |>
      httr2::req_perform(verbosity = 3)
    
    # the data is in Content-Type: text/plain format
    spectral_data <- httr2::resp_body_string(response, encoding = "UTF-8")
    
    df <- read.table(text = spectral_data, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    df$level2.reflectance <- gsub("\\[|\\]", "", df$level2.reflectance)
    df$level2.reflectance <- strsplit(df$level2.reflectance, ",")
    
    if (df$measurement.id[2] == "-1") {
      # check if the exist data for other station in the same data provided
      response_no_station <- httr2::request("https://wispcloud.waterinsight.nl/api/query") |> 
        httr2::req_url_query(
          SERVICE = "data",
          VERSION = version,
          REQUEST = "GetData",
          TIME = paste(time_from, time_to, sep = ","),
          INCLUDE = "measurement.id,measurement.date,instrument.name,level2.quality,ed.selected,lu.selected,ld.selected,waterquality.tsm,waterquality.chla,waterquality.kd,waterquality.cpc,level2.reflectance"
        ) |> 
        httr2::req_auth_basic(userid, pwd) |>
        httr2::req_perform(verbosity = 3)
      
      # the data is in Content-Type: text/plain format
      spectral_data_no_station <- httr2::resp_body_string(response_no_station, encoding = "UTF-8")
      
      df_no_station <- read.table(text = spectral_data_no_station, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      df_no_station$level2.reflectance <- gsub("\\[|\\]", "", df_no_station$level2.reflectance)
      df_no_station$level2.reflectance <- strsplit(df_no_station$level2.reflectance, ",")
      
      if (df_no_station$measurement.id[2] == "-1") {
        reflectance_data_tbl <- NULL
        message("\n----\nThank you for your request, but the instrument does not acquire data on this date.\n----\n")
      } else {
        reflectance_data_tbl <- NULL
        new_station <- df_no_station$instrument.name[2]
        message(paste0(
          "\n----\nThank you for your request. Data for the requested station is not available, but we know there is data for the same date from this station: ",
          new_station,
          ".\n",
          "Maybe you're interested in these?\n\nResubmit the request by entering this value ",
          new_station,
          " in the 'station' parameter.\n----\n"
        ))
      }
    } else {
      reflectance_data_tbl <- tibble::as_tibble(df) |>
        dplyr::slice(-1) |>
        tidyr::unnest_wider(
          col = level2.reflectance,
          names_sep = ("_")
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::starts_with("level2.reflectance_"), ~ units::set_units(as.numeric(as.character(.)), "1/sr")
          ),
          dplyr::across(
            dplyr::starts_with("waterquality."), ~ as.numeric(as.character(.))
          ),
          measurement.date = lubridate::as_datetime(measurement.date),
          waterquality.tsm = units::set_units(waterquality.tsm, "g/m3"),
          waterquality.chla = units::set_units(waterquality.chla, "mg/m3"),
          waterquality.kd = units::set_units(waterquality.kd, "1/m"),
          waterquality.cpc = units::set_units(waterquality.cpc, "mg/m3"),
          
          level2.quality = as.character(level2.quality) # Conversion without units of measurement
        ) |>
       
        dplyr::rename_with(
          ~ stringr::str_c(
            # 'level2.reflectance_nm_',
            "nm_",
            350:900
          ),
          dplyr::starts_with("level2.reflectance_")
        )
    }
  } else {
    reflectance_data_tbl <- NULL
    message("\n----\nPlease check the 'time_from' and 'time_to' parameters.\n\nThe two dates are not consistent.\n\nThe dates must be equal. \n\nTo use multiple dates use: reflect_data <- wisp_get_reflectance_multi_data\n----\n")
  }
  reflectance_data_tbl
}

#' Get data of reflectance (level2) from WISPstation for a multiple dates
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the data of reflectance from WISPstation for a
#' multiple dates.
#' @param version A `character`. It is the version of the API data. Default
#' is "1.0"
#' @param time_from A `character`. It is the date and time from which the data
#' is requested.
#' @param time_to A `character`. It is the date and time to which the data
#' is requested.
#' @param station A `character`.  It is the name of the station.
#' @param userid A `character`. It is the userid to access to the data service.
#' @param pwd A `character`. It is the password to access to the data service.
#' @return A `tibble` with measurement id, measurement date, instrument name,
#' level2_quality, set of sensor (irradiance and radiances),
#' waterquality values of TSM, Chla, Kd, and cpc as provided by instrument by
#' default, all the values of reflectance for each wevelength from 350 to 900
#' nm.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data <- WISP.data::wisp_get_reflectance_multi_data(
#'   time_from = "2024-04-08T09:00",
#'   time_to = "2024-04-10T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#' }
#' ## End (Not run)
#' 
### wisp_get_reflectance_multi_data
wisp_get_reflectance_multi_data <- function(
    version = "1.0",
    time_from = NULL,
    time_to = NULL,
    station = NULL,
    userid = NULL,
    pwd = NULL
) {
  from_time <- sub(".*T", "", time_from)
  to_time <- sub(".*T", "", time_to)
  data_multiDates <- lapply(
    seq.Date(
      from = as.Date(time_from),
      to = as.Date(time_to),
      by = "day"
    ), function(date) {
    wisp_get_reflectance_data(
      version = version,
      time_from = paste0(date, "T", from_time),
      time_to = paste0(date, "T", to_time),
      station = station,
      userid = userid,
      pwd = pwd
    )
  }) |>
    dplyr::bind_rows()
    return(data_multiDates)
}

#' Quality Control (QC) for WISPstation reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function removes all anomalous spectral signatures and explains the reason for each elimination
#' @param data A `tibble`. From wisp_get_reflectance_data() function.
#' @param maxPeak A `decimal`. Maximum magnitude of the spectral signatures.
#' We recommend setting this parameter to: 0.02 for clear and oligotrophic water,
#' 0.05 for meso- to eutrophic water, and 0.08 for hypereutrophic and highly turbid water.
#' #' Default is 0.05.
#' @return A `tibble` with the spectral signatures that have passed QC operation. In addition,
#' a message containing the reason behind the elimination of each anomalous spectral signature
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr filter if_all all_of rowwise ungroup mutate across
#' @importFrom dplyr starts_with c_across
#' @importFrom units set_units
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data_qc <- WISP.data::wisp_qc_reflectance_data(data = reflect_data, maxPeak = 0.05)
#' }
#' ## End (Not run)
#' 
### wisp_qc_reflectance_data
wisp_qc_reflectance_data <- function(data, maxPeak = 0.05) {
  initial_nrow <- nrow(data)
  removed_rows <- data.frame(measurement.date = data$measurement.date, reason = "")
  
  # QC1 -> Removal lines with negative values below 845 nm
  columns_nm_below_845 <- grep("^nm_([0-7][0-9]{2}|8[0-3][0-9]|84[0-4])", colnames(data), value = TRUE)
  data[columns_nm_below_845] <- lapply(data[columns_nm_below_845], as.numeric)
  removed_QC1 <- data[rowSums(data[columns_nm_below_845] < 0, na.rm = TRUE) > 0, ]
  removed_rows$reason[data$measurement.date %in% removed_QC1$measurement.date] <- " QC1"
  
  # QC2 -> Removal lines with outliers in the NIR (840 nm > 700 nm)
  data$nm_700 <- as.numeric(data$nm_700)
  data$nm_840 <- as.numeric(data$nm_840)
  removed_QC2 <- data[data$nm_840 > data$nm_700, ]
  removed_rows$reason[data$measurement.date %in% removed_QC2$measurement.date] <- 
    paste(removed_rows$reason[data$measurement.date %in% removed_QC2$measurement.date], "QC2", sep = " ")
  
  # QC3 -> Removal lines with maximum peak greater than "maxPeak"
  columns_nm <- grep("^nm_", colnames(data), value = TRUE)
  data[columns_nm] <- lapply(data[columns_nm], as.numeric)
  removed_QC3 <- data[apply(data[columns_nm], 1, max) > maxPeak, ]
  removed_rows$reason[data$measurement.date %in% removed_QC3$measurement.date] <- 
    paste(removed_rows$reason[data$measurement.date %in% removed_QC3$measurement.date], "QC3", sep = " ")
  
  # QC4 -> Removal lines with outliers in the Blue domain
  removed_QC4 <- data[which(
    data$nm_350 > pmax(data$nm_555,
                       data$nm_560,
                       data$nm_565,
                       data$nm_570,
                       data$nm_575) &
      pmax(data$nm_555,
           data$nm_560,
           data$nm_565,
           data$nm_570,
           data$nm_575) > data$nm_495
  ), ]
  removed_rows$reason[data$measurement.date %in% removed_QC4$measurement.date] <- 
    paste(removed_rows$reason[data$measurement.date %in% removed_QC4$measurement.date], "QC4", sep = " ")
  
  # QC5 -> Removal of lines similar to "decreasing logarithms"
  columns_nm_range <- grep("^nm_(3[5-9][0-9]|4[0-9]{2}|500)$", colnames(data), value = TRUE)
  removed_QC5 <- data |> 
    dplyr::rowwise() |> 
    dplyr::filter({
      valori <- dplyr::c_across(dplyr::all_of(columns_nm_range))
      diff_values <- diff(valori)
      percentage_negative <- mean(diff_values < 0, na.rm = TRUE)
      percentage_negative >= 0.95
    }) |> 
    dplyr::ungroup()
  removed_rows$reason[data$measurement.date %in% removed_QC5$measurement.date] <- 
    paste(removed_rows$reason[data$measurement.date %in% removed_QC5$measurement.date], "QC5", sep = " ")
  
  # QC6 -> Removal of "invalid" and "None" lines (level2.quality)
  removed_QC6 <- data[!(data$level2.quality %in% c("okay", "suspect")), ]
  removed_rows$reason[data$measurement.date %in% removed_QC6$measurement.date] <- 
    paste(trimws(removed_rows$reason[data$measurement.date %in% removed_QC6$measurement.date]), "QC6", sep = " ")
  
  # Removal of outlier spectral signatures
  reflectance_data_filtered <- data[removed_rows$reason == "", ]
  
  # Output message
  final_nrow <- nrow(reflectance_data_filtered)
  removed_count <- initial_nrow - final_nrow
  removed_rows_summary <- removed_rows[removed_rows$reason != "", ]
  removal_summary <- table(trimws(removed_rows_summary$reason))
  
  qc_descriptions <- c(
    QC1 = "remove spectral signatures with negative values below 845 nm",
    QC2 = "remove spectral signatures with outliers in the NIR (840 nm > 700 nm)",
    QC3 = "remove spectral signatures with maximum peak greater than maxPeak",
    QC4 = "remove spectral signatures with outliers in the Blue domain",
    QC5 = "remove spectral signatures similar to 'decreasing logarithms' functions",
    QC6 = "remove 'invalid' and 'None' spectral signatures according to level2.quality"
  )
  
  message("\n----")
  message(removed_count, " spectral signatures were removed during QC:\n")
  
  for (qc_combo in names(removal_summary)) {
    message("- ", removal_summary[qc_combo], " spectral signatures were removed thanks to ", gsub(" ", "+", qc_combo))
  }
  
  message("")
  
  for (i in seq_len(nrow(removed_rows_summary))) {
    message("The spectral signature of ", sub("\\..*", "", removed_rows_summary$measurement.date[i]),
            " has been removed thanks to: ", trimws(removed_rows_summary$reason[i]))
  }
  
  message("")
  
  used_qc <- unique(unlist(strsplit(trimws(paste(removed_rows_summary$reason, collapse = " ")), "\\s+")))
  used_qc <- sort(used_qc)
  for (qc in used_qc) {
    message(qc, " ", qc_descriptions[qc])
  }
  
  message("----\n")
  
  if (final_nrow == 0) {
    message("Thank you for your request, but the QC operation removed all the spectral signatures available on this date.")
    return(NULL)
  }
  
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("nm_"), ~ units::set_units(as.numeric(as.character(.)), "1/sr")
      )
    )
  
  return(reflectance_data_filtered)
}

#' SUNGLINT Removal (SR) for WISPstation reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function applies the algorithm of Jiang et al., (2020) for removing
#' sunglint from spectral signatures
#' @param qc_data A `tibble` from wisp_qc_reflectance_data() function.
#' @param save_output A `logical`. If `TRUE`, the function saves the output in a
#' CSV file. Default is `FALSE`.
#' @return A tibble with the spectral signatures after the SR operation and,
#' if parameter `save_output` is `TRUE`, the function saves the reflectance data
#' in a CSV file.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate select all_of across
#' @importFrom units set_units
#' @importFrom readr write_csv
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data_sr <- WISP.data::wisp_sr_reflectance_data(
#'   qc_data = reflect_data_qc,
#'   save_output = FALSE
#' ) 
#' }
#' ## End (Not run)
#'
### wisp_sr_reflectance_data
wisp_sr_reflectance_data <- function(qc_data, save_output = FALSE, output_dir = getwd()) {
  columns_750_780 <- grep("^nm_(750|751|752|753|754|755|756|757|758|759|760|761|762|763|764|765|766|767|768|769|770|771|772|773|774|775|776|777|778|779|780)$", 
                          colnames(qc_data), value = TRUE)
  columns_780 <- grep("^nm_(775|776|777|778|779|780|781|782|783|784|785)$", colnames(qc_data), value = TRUE)
  columns_810 <- grep("^nm_(805|806|807|808|809|810|811|812|813|814|815)$", colnames(qc_data), value = TRUE)
  columns_840 <- grep("^nm_(835|836|837|838|839|840|841|842|843|844|845)$", colnames(qc_data), value = TRUE)
  columns_nm <- grep("^nm_", colnames(qc_data), value = TRUE)
  
  corrected_data <- qc_data |>
    # Calculation of the median between 750 and 780 nm ("md_750_780")
    dplyr::mutate(
      md_750_780 = apply(dplyr::select(qc_data, dplyr::all_of(columns_750_780)), 1, median, na.rm = TRUE),
      # Calculation of the median at 780, 810 and 840 nm (± 5 nm) ("md_780", "md_810", "md_840")
      md_780 = apply(dplyr::select(qc_data, dplyr::all_of(columns_780)), 1, median, na.rm = TRUE),
      md_810 = apply(dplyr::select(qc_data, dplyr::all_of(columns_810)), 1, median, na.rm = TRUE),
      md_840 = apply(dplyr::select(qc_data, dplyr::all_of(columns_840)), 1, median, na.rm = TRUE),
      # Calculation of "RHW"
      RHW = md_810 - md_780 - ((md_840 - md_780) * (810.0 - 780.0) / (840.0 - 780.0)),
      # Calculation of "est_md_750_780" (median estimate between 750 and 780 nm)
      est_md_750_780 = 18267.884 * RHW^3 - 129.158 * RHW^2 + 3.072 * RHW,
      # Calculation of "delta" based on "RHW" value
      delta = units::set_units(ifelse(RHW > 0, md_750_780 - est_md_750_780, md_750_780), "1/sr"),
      # Rrs correction based on "delta"
      dplyr::across(
        dplyr::all_of(columns_nm), 
        ~ . - delta
      )
    )
  
  # Date range info
  date_from <- format(as.Date(corrected_data$measurement.date[1]), "%Y%m%d")
  date_to <- format(as.Date(corrected_data$measurement.date[nrow(corrected_data)]), "%Y%m%d")
  
  # Determines the filename
  dates <- if (identical(date_from, date_to)) date_from else paste0(date_from, "_", date_to)
  
  # Save data in CSV file if save_output is TRUE
  if (save_output) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    output_file <- file.path(output_dir, paste0("reflectance_data_sr_", dates, ".csv"))
    readr::write_csv(corrected_data, output_file)
    
    cat("\n----\nThe reflectance data has been saved as:\n", output_file, "\n----\n")
  }
  
  # Return function output
  return(corrected_data)
}

#' Scattering peak for WISPstation reflectance data  (UNITA' DI MISURAAAAA)
#' @description `r lifecycle::badge("experimental")`
#' This function calculates the peak between 690 and 710 nm (scattering) 
#' for each spectral signatures and adds a new column in "reflect_data_sr"
#' @param sr_data A `tibble` from wisp_sr_reflectance_data() function. 
#' @return 
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom 
#' @export
#' @examples
#' # example code
#' \dontrun{
### wisp_scattering_peak
wisp_scattering_peak <- function(sr_data) {
  
  columns_690_710 <- grep("^nm_(69[0-9]|70[0-9]|710)$", colnames(sr_data), value = TRUE)
  
  sr_data <- sr_data |>
    dplyr::mutate(
      scattering.peak = units::set_units(as.numeric(
        apply(dplyr::select(sr_data, dplyr::all_of(columns_690_710)), 1, max, na.rm = TRUE)), "1/sr")
    ) |>
    dplyr::relocate(scattering.peak, .after = waterquality.chla)
    
  return(sr_data)
}

#' Band ratio for WISPstation reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function calculates the band ratio between the peak between 690 and 
#' 710 nm and the peak between 670 e 680 nm 
#' (??????????????????????????????????????????????????????????) 
#' @param sr_data A `tibble` from wisp_sr_reflectance_data() function.
#' @return 
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom 
#' @export
#' @examples
#' # example code
#' \dontrun{
### wisp_band_ratio
wisp_band_ratio <- function(sr_data) {
  
  columns_690_710 <- grep("^nm_(69[0-9]|70[0-9]|710)$", colnames(sr_data), value = TRUE)
  columns_670_680 <- grep("^nm_(67[0-9]|680)$", colnames(sr_data), value = TRUE)
  
  sr_data <- sr_data |>
    dplyr::mutate(
      scattering.peak = units::set_units(as.numeric(
        apply(dplyr::select(sr_data, dplyr::all_of(columns_690_710)), 1, max, na.rm = TRUE)), "1/sr"),
      band.ratio = units::set_units(scattering.peak / apply(dplyr::select(sr_data, dplyr::all_of(columns_670_680)), 1, max, na.rm = TRUE), "")
    ) |>
    dplyr::relocate(band.ratio, .after = scattering.peak)
  
  return(sr_data)
}

#' Calculation of SPM concentration (Novoa et al., 2017)
#' @description `r lifecycle::badge("experimental")`
#' This function calculates the SPM concentration in according 
#' to Novoa et al., 2017 and creates new columns in qc_data e sr_data
#' @param qc_data A `tibble` from wisp_qc_reflectance_data() function.
#' @param sr_data A `tibble` from wisp_sr_reflectance_data() function.
#' @return 
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate relocate rowwise ungroup select
#' @importFrom purrr map_dbl map_chr
#' @export
#' @examples
#' # example code
#' \dontrun{
### wisp_novoa_SPM
wisp_novoa_SPM <- function(qc_data, sr_data) {
  
  novoa_spm_dict <- list(
    novoa_waves_req = c(560, 665, 865),   
    novoa_algorithm = "nechad_centre",     # "nechad_centre" or "nechad_average"
    novoa_output_switch = FALSE,           
    rhow_switch_red = c(0.007, 0.016),     
    rhow_switch_nir = c(0.08, 0.12)        #  or "c(0.046,0.09)" (Bourgneuf) 
  )
  
  # A_Nechad and C_Nechad coefficients (SPM) for green, red and nir bands
  get_nechad_spm_coefficients <- function(wave) {
    if (wave == 560) {
      return(list(A = 104.2, C = 0.1449))
    } else if (wave == 665) {
      return(list(A = 355.85, C = 0.1728))
    } else if (wave == 865) {
      return(list(A = 2971.93, C = 0.2115))
    } else {
      stop("Wavelength not supported")
    }
  }
  
  # SPM concentration algorithm (Nechad et al., 2010)
  calculate_spm <- function(rhow, A, C) {
    spm <- (A * rhow) / (1 - (rhow / C))
    return(spm)
  }
  
  # Identification of wavelengths (+/- 3 nm)
  get_columns_in_range <- function(df, center, tol = 3) {
    nm_cols <- names(df)[grepl('^nm_', names(df))]
    wavelengths <- as.numeric(gsub('nm_', '', nm_cols))
    selected_cols <- nm_cols[wavelengths >= (center - tol) & wavelengths <= (center + tol)]
    return(selected_cols)
  }
  
  # SPM concentration algorithm (Novoa et al., 2017) + Blending
  compute_novoa_spm <- function(green_value, red_value, nir_value) {
    coeff_green <- get_nechad_spm_coefficients(560)
    coeff_red   <- get_nechad_spm_coefficients(665)
    coeff_nir   <- get_nechad_spm_coefficients(865)
    
    spm_green <- calculate_spm(green_value, coeff_green$A, coeff_green$C)
    spm_red   <- calculate_spm(red_value,   coeff_red$A,   coeff_red$C)
    spm_nir   <- calculate_spm(nir_value,   coeff_nir$A,   coeff_nir$C)
    
    rhow_red_lower <- novoa_spm_dict$rhow_switch_red[1]
    rhow_red_upper <- novoa_spm_dict$rhow_switch_red[2]
    rhow_nir_lower <- novoa_spm_dict$rhow_switch_nir[1]
    rhow_nir_upper <- novoa_spm_dict$rhow_switch_nir[2]
    
    if (red_value < rhow_red_lower) {
      spm_final <- spm_green
      band_selected <- "Green (560nm)"
    } else if (red_value < rhow_red_upper) {
      logb <- log(rhow_red_upper / rhow_red_lower)
      spm_final <- spm_green * log(rhow_red_upper / red_value) / logb +
        spm_red * log(red_value / rhow_red_lower) / logb
      band_selected <- "Blending green and red"
    } else if (red_value < rhow_nir_lower) {
      spm_final <- spm_red
      band_selected <- "Red (665nm)"
    } else if (red_value < rhow_nir_upper) {
      logb <- log(rhow_nir_upper / rhow_nir_lower)
      spm_final <- spm_red * log(rhow_nir_upper / nir_value) / logb +
        spm_nir * log(nir_value / rhow_nir_lower) / logb
      band_selected <- "Blending red and NIR"
    } else {
      spm_final <- spm_nir
      band_selected <- "NIR (865nm)"
    }
    
    return(list(SPM = spm_final, band_selected = band_selected))
  }
  
  # --- SPM calculation for "qc_data" ---
  green_cols_qc <- get_columns_in_range(qc_data, 560, tol = 3)
  red_cols_qc   <- get_columns_in_range(qc_data, 665, tol = 3)
  nir_cols_qc   <- get_columns_in_range(qc_data, 865, tol = 3)
  
  qc_data <- qc_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      green_value = as.numeric(mean(dplyr::c_across(all_of(green_cols_qc)), na.rm = TRUE) * pi),
      red_value   = as.numeric(mean(dplyr::c_across(all_of(red_cols_qc)), na.rm = TRUE) * pi),
      nir_value   = as.numeric(mean(dplyr::c_across(all_of(nir_cols_qc)), na.rm = TRUE) * pi),
      novoa_spm = list(compute_novoa_spm(green_value, red_value, nir_value))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Novoa_SPM      = paste0(round(purrr::map_dbl(novoa_spm, "SPM"), 1), " [g/m3]"),
      Blended_SPM = purrr::map_chr(novoa_spm, "band_selected")
    ) |>
    dplyr::relocate(Novoa_SPM, Blended_SPM, .after = waterquality.tsm) |>
    dplyr::select(-green_value, -red_value, -nir_value, -novoa_spm)
  
  # --- SPM calculation for "sr_data" ---
  green_cols_sr <- get_columns_in_range(sr_data, 560, tol = 3)
  red_cols_sr   <- get_columns_in_range(sr_data, 665, tol = 3)
  nir_cols_sr   <- get_columns_in_range(sr_data, 865, tol = 3)
  
  sr_data <- sr_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      green_value = as.numeric(mean(dplyr::c_across(all_of(green_cols_sr)), na.rm = TRUE) * pi),
      red_value   = as.numeric(mean(dplyr::c_across(all_of(red_cols_sr)), na.rm = TRUE) * pi),
      nir_value   = as.numeric(mean(dplyr::c_across(all_of(nir_cols_sr)), na.rm = TRUE) * pi),
      novoa_spm = list(compute_novoa_spm(green_value, red_value, nir_value))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Novoa_SPM      = paste0(round(purrr::map_dbl(novoa_spm, "SPM"), 1), " [g/m3]"),
      Blended_SPM = purrr::map_chr(novoa_spm, "band_selected")
    ) |>
    dplyr::relocate(Novoa_SPM, Blended_SPM, .after = waterquality.tsm) |>
    dplyr::select(-green_value, -red_value, -nir_value, -novoa_spm)
  
  return(list(qc_data = qc_data, sr_data = sr_data))
}

#' Calculation of Turbidity (FNU) (Novoa et al., 2017)
#' @description `r lifecycle::badge("experimental")`
#' This function calculates the Turbidity (FNU) in according 
#' to Novoa et al., 2017 and creates new columns in qc_data e sr_data
#' @param qc_data A `tibble` from wisp_qc_reflectance_data() function.
#' @param sr_data A `tibble` from wisp_sr_reflectance_data() function.
#' @return 
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate rowwise ungroup relocate select
#' @importFrom purrr map_dbl map_chr
#' @export
#' @examples
#' # example code
#' \dontrun{
### wisp_novoa_TUR
wisp_novoa_TUR <- function(qc_data, sr_data) {
  
  novoa_tur_dict <- list(
    novoa_waves_req   = c(665, 865),          
    novoa_algorithm   = "nechad_centre",      # "nechad_centre" or "nechad_average"
    novoa_output_switch = FALSE,
    rhow_switch_nir   = c(0.08, 0.12)         #  or "c(0.046,0.09)" (Bourgneuf) 
  )
  
  # A_Nechad and C_Nechad coefficients (TUR) for green, red and nir bands
  get_nechad_tur_coefficients <- function(wave) {
    if (wave == 665) {
      return(list(A = 282.95, C = 0.1728))
    } else if (wave == 865) {
      return(list(A = 2109.35, C = 0.2115))
    } else {
      stop("Wavelength not supported")
    }
  }
  
  # Turbidity algorithm (Nechad et al., 2010)
  calculate_turbidity <- function(rhow, A, C) {
    tur <- (A * rhow) / (1 - (rhow / C))
    return(tur)
  }
  
  # Identification of wavelengths (+/- 3 nm)
  get_columns_in_range <- function(df, center, tol = 3) {
    nm_cols <- names(df)[grepl('^nm_', names(df))]
    wavelengths <- as.numeric(gsub('nm_', '', nm_cols))
    selected_cols <- nm_cols[wavelengths >= (center - tol) & wavelengths <= (center + tol)]
    return(selected_cols)
  }
  
  # Turbidity algorithm (Novoa et al., 2017) + Blending
  compute_novoa_tur <- function(red_value, nir_value) {
    coeff_red <- get_nechad_tur_coefficients(665)
    coeff_nir <- get_nechad_tur_coefficients(865)
    
    red_tur <- calculate_turbidity(red_value, coeff_red$A, coeff_red$C)
    nir_tur <- calculate_turbidity(nir_value, coeff_nir$A, coeff_nir$C)
    
    lower <- novoa_tur_dict$rhow_switch_nir[[1]]  
    upper <- novoa_tur_dict$rhow_switch_nir[[2]]  
    
    if (red_value <= lower) {
      tur_final <- red_tur
      band_selected <- "Red (665nm)"
    } else if (red_value >= upper) {
      tur_final <- nir_tur
      band_selected <- "NIR (865nm)"
    } else {
      logb <- log(upper / lower)
      tur_final <- red_tur * log(upper / red_value) / logb +
        nir_tur * log(red_value / lower) / logb
      band_selected <- "Blending red and NIR"
    }
    
    return(list(TUR = tur_final, band_selected = band_selected))
  }
  
 
  # --- TUR calculation for "qc_data" ---
  red_cols_qc <- get_columns_in_range(qc_data, 665, tol = 3)
  nir_cols_qc <- get_columns_in_range(qc_data, 865, tol = 3)
  
  qc_data <- qc_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      red_value = as.numeric(mean(dplyr::c_across(all_of(red_cols_qc)), na.rm = TRUE) * pi),
      nir_value = as.numeric(mean(dplyr::c_across(all_of(nir_cols_qc)), na.rm = TRUE) * pi),
      novoa_tur = list(compute_novoa_tur(red_value, nir_value))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Novoa_TUR      = paste0(round(purrr::map_dbl(novoa_tur, "TUR"), 1), " [NTU]"),
      Blended_TUR = purrr::map_chr(novoa_tur, "band_selected")
    ) |>
    dplyr::relocate(Novoa_TUR, Blended_TUR, .after = Blended_SPM) |>
    dplyr::select(-red_value, -nir_value, -novoa_tur)
  
  # --- TUR calculation for "sr_data" ---
  red_cols_sr <- get_columns_in_range(sr_data, 665, tol = 3)
  nir_cols_sr <- get_columns_in_range(sr_data, 865, tol = 3)
  
  sr_data <- sr_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      red_value = as.numeric(mean(dplyr::c_across(all_of(red_cols_sr)), na.rm = TRUE) * pi),
      nir_value = as.numeric(mean(dplyr::c_across(all_of(nir_cols_sr)), na.rm = TRUE) * pi),
      novoa_tur = list(compute_novoa_tur(red_value, nir_value))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Novoa_TUR      = paste0(round(purrr::map_dbl(novoa_tur, "TUR"), 1), " [NTU]"),
      Blended_TUR = purrr::map_chr(novoa_tur, "band_selected")
    ) |>
    dplyr::relocate(Novoa_TUR, Blended_TUR, .after = Blended_SPM) |>
    dplyr::select(-red_value, -nir_value, -novoa_tur)
  
  return(list(qc_data = qc_data, sr_data = sr_data))
}

#' Create a plot of reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function return a plotly of each spectral signature measured by a
#' WISPstation.
#' @param data A `tibble` obtained by any of the functions provided by this
#' package: wisp_get_reflectance_data(), or after QC and SR removal operations.
#' @param legend_TSM A `logical`. If `TRUE`, the plot legend includes the `TSM`
#' values. Default is `TRUE`.
#' @param legend_Chla A `logical`. If `TRUE`, the plot legend includes the
#' `Chla` values. Default is `TRUE`.
#' @param legend_Kd A `logical`. If `TRUE`, the plot legend includes the `Kd`
#' values. Default is `TRUE`.
#' @param legend_cpc A `logical`. If `TRUE`, the plot legend includes the `cpc`
#' values. Default is `TRUE`.
#' @return description A plotly object with the spectral signatures of the
#' reflectance data.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom viridis viridis
#' @importFrom plotly plot_ly layout
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' WISP.data::wisp_plot_reflectance_data(
#'   data = reflect_data_sr,
#'   legend_TSM = FALSE,
#'   legend_Chla = TRUE,
#'   legend_Kd = FALSE,
#'   legend_cpc = FALSE
#' )
#' }
#' ## End (Not run)
#'
### wisp_plot_reflectance_data
wisp_plot_reflectance_data <- function(
    data,
    legend_TSM = TRUE,
    legend_Chla = TRUE,
    legend_Kd = TRUE,
    legend_cpc = TRUE
) {
  # Production of data information
  data <- data |>
    dplyr::mutate(
      products_info = mapply(function(tsm, chla, kd, cpc) {
        paste(
          c(
            if (legend_TSM) paste("TSM [g/m3]:", tsm),
            if (legend_Chla) paste("Chla [mg/m3]:", chla),
            if (legend_Kd) paste("Kd [1/m]:", kd),
            if (legend_cpc) paste("cpc [mg/m3]:", cpc)
          ),
          collapse = "<br>"
        )
      }, waterquality.tsm, waterquality.chla, waterquality.kd, waterquality.cpc)
    )
  
  # Data transformation
  data_2 <- data |>
    dplyr::select(
      measurement.date, starts_with("nm_"), products_info
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("nm_"),
      names_to = "wavelength",
      values_to = "Rrs"
    ) |>
    dplyr::mutate(
      wavelength = as.numeric(sub("nm_", "", wavelength)),
      date_time_info = paste0(
        "Date: ", substr(measurement.date, 1, 10),
        "<br>Time [UTC]: ", substr(measurement.date, 12, 19)
      ),
      legend_info = paste(date_time_info, products_info, sep = "<br>")
    )
  
  # Color palette
  num_colors <- length(unique(data_2$legend_info))
  color_palette <- viridis::viridis(num_colors)
  
  # Plot title
  length_dates <- length(data$measurement.date)
  n_dates <- seq.Date(
    from = as.Date(data$measurement.date[1]),
    to = as.Date(data$measurement.date[length_dates]),
    by = "day"
  )
  dates_text <- if (length(n_dates) > 1) {
    paste0("<br>from date: ", n_dates[1], "<br>to date: ", n_dates[length(n_dates)])
  } else {
    paste0("<br>on the date: ", n_dates[1])
  }
  
  # Plotly
  fig <- plotly::plot_ly(
    data_2,
    x = ~wavelength,
    y = ~Rrs,
    color = ~legend_info,
    text = ~legend_info,
    hovertemplate = paste0(
      "Wavelength: %{x} nm<br>",
      "Rrs: %{y:.4f} [1/sr]<br>",
      "%{text}<extra></extra>"
    ),
    colors = color_palette,
    type = 'scatter',
    mode = 'lines'
  ) |>
    plotly::layout(
      title = paste0("Aquired by: ", data$instrument.name[1], dates_text),
      xaxis = list(title = '<b>Wavelength [nm]<b>', dtick = 100),
      yaxis = list(title = '<b>Rrs [1/sr]<b>'),
      legend = list(title = list(text = '<b>Time of acquisition</b>'))
    )
  
  fig
}

#' Creates a plot of the trend of one or more parameters
#' @description `r lifecycle::badge("experimental")`
#' This function creates a graph related to the time trend of one or 
#' more water quality parameters associated with spectral signatures filtered 
#' and corrected by sunglint
#' @param sr_data A `tibble` from wisp_sr_reflectance_data() function.
#' @return 
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom 
#' @export
#' @examples
#' # example code
#' \dontrun{
### wisp_trend_plot

