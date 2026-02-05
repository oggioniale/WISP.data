#' Get data of reflectance (level2) from WISPstation for a specific date
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the reflectance data from WISPstation for a
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
#' @param save_csv A `logical`. If `TRUE`, the function saves the reflectance data.
#' @param out_dir A `character`. The directory where the CSV file will be saved.
#' Default is "outputs" within the working directory.
#' @return A `tibble` with measurement id, measurement date, instrument name,
#' level2_quality, set of sensor (irradiance and radiances),
#' waterquality values of TSM, Chla, Kd, and cpc as provided by instrument by default,
#' all the reflectance values from 350 to 900 nm.
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
#' reflect_data <- wisp_get_reflectance_data(
#'   time_from = "2024-09-01T09:00",
#'   time_to = "2024-09-01T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' 
#' # with data
#' reflect_data <- wisp_get_reflectance_data(
#'   time_from = "2024-08-01T09:00",
#'   time_to = "2024-08-01T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#'
#' # no data for the station selected
#' reflect_data <- wisp_get_reflectance_data(
#'   time_from = "2019-06-20T09:00",
#'   time_to = "2019-06-20T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' 
#' # The two dates are not consistent
#' reflect_data <- wisp_get_reflectance_data(
#'   time_from = "2019-06-20T09:00",
#'   time_to = "2020-06-20T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' }
#' ## End (Not run)
#' 
### wisp_get_reflectance_data
wisp_get_reflectance_data <- function(
    version   = "1.0",
    time_from = NULL,
    time_to   = NULL,
    station   = NULL,
    userid    = NULL,
    pwd       = NULL,
    save_csv  = FALSE,
    out_dir   = "outputs"
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
        INCLUDE = "measurement.id,measurement.date,instrument.name,measurement.latitude,measurement.longitude,level2.quality,ed.selected,lu.selected,ld.selected,waterquality.tsm,waterquality.chla,waterquality.kd,waterquality.cpc,level2.reflectance"
      ) |> 
      httr2::req_auth_basic(userid, pwd) |>
      httr2::req_perform(verbosity = 3)
    
    # the data is in Content-Type: text/plain format
    spectral_data <- httr2::resp_body_string(response, encoding = "UTF-8")
    
    df <- read.table(text = spectral_data, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    df$level2.reflectance <- gsub("\\[|\\]", "", df$level2.reflectance)
    df$level2.reflectance <- strsplit(df$level2.reflectance, ",")
    
    if (df$measurement.id[2] == "-1" ||
        all(df$level2.reflectance[-1] %in% c("None", NA))
      ) {
      # check if the exist data for other station in the same data provided
      response_no_station <- httr2::request("https://wispcloud.waterinsight.nl/api/query") |> 
        httr2::req_url_query(
          SERVICE = "data",
          VERSION = version,
          REQUEST = "GetData",
          TIME = paste(time_from, time_to, sep = ","),
          INCLUDE = "measurement.id,measurement.date,instrument.name,measurement.latitude,measurement.longitude,level2.quality,ed.selected,lu.selected,ld.selected,waterquality.tsm,waterquality.chla,waterquality.kd,waterquality.cpc,level2.reflectance"
        ) |> 
        httr2::req_auth_basic(userid, pwd) |>
        httr2::req_perform(verbosity = 3)
      
      # the data is in Content-Type: text/plain format
      spectral_data_no_station <- httr2::resp_body_string(response_no_station, encoding = "UTF-8")
      
      df_no_station <- read.table(text = spectral_data_no_station, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      df_no_station$level2.reflectance <- gsub("\\[|\\]", "", df_no_station$level2.reflectance)
      df_no_station$level2.reflectance <- strsplit(df_no_station$level2.reflectance, ",")
      
      if (df_no_station$measurement.id[2] == "-1" ||
          all(df$level2.reflectance[-1] %in% c("None", NA))
        ) {
        reflectance_data_tbl <- NULL
        message(
          "\n----\n⚠️ Thank you for your request, but the instrument does not acquire data on ",
          start_date,
          ".\nIf you requested data for more than one date, only the data for ", start_date, " will be missing in the result.\n",
          "If you requested data for only one date, your result will be empty.",
          "\n----\n"
        )
      } else {
        reflectance_data_tbl <- NULL
        new_station <- df_no_station$instrument.name[2]
        message(paste0(
          "\n----\n⚠️ Thank you for your request. Data for the requested station is not available, but we know there is data for the same date from this station: ",
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
          measurement.latitude = units::set_units(as.numeric(measurement.latitude), degree),
          measurement.longitude = units::set_units(as.numeric(measurement.longitude), degree),
          waterquality.tsm = units::set_units(waterquality.tsm, "g/m3"),
          waterquality.chla = units::set_units(waterquality.chla, "mg/m3"),
          waterquality.kd = units::set_units(waterquality.kd, "1/m"),
          waterquality.cpc = units::set_units(waterquality.cpc, "mg/m3"),
          level2.quality = as.character(level2.quality) # Conversion without units of measurement
        ) |>
        dplyr::rename_with(
          ~ stringr::str_c(
            "nm_",
            350:900
          ),
          dplyr::starts_with("level2.reflectance_")
        )
    }
    # create output csv file
    if (save_csv) {
      dates <- if (identical(start_date, end_date)) start_date else paste0(start_date, "_", end_date)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
      }
      file <- file.path(out_dir, paste0("reflectance_data_", dates, ".csv"))
      readr::write_csv(x = reflectance_data_tbl, file = file)
    }
  } else {
    reflectance_data_tbl <- NULL
    message("\n----\n⚠️ Please check the 'time_from' and 'time_to' parameters.\n\nThe two dates are not consistent.\n\nThe dates must be equal. \n\nTo use multiple dates use function: wisp_get_reflectance_multi_data()\n----\n")
  }
  # output
  return(reflectance_data_tbl)
}

#' Get data of reflectance (level2) from WISPstation for multiple dates
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the reflectance data from WISPstation for
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
#' @param save_csv A `logical`. If `TRUE`, the function saves the reflectance data.
#' @param out_dir A `character`. The directory where the CSV file will be saved.
#' Default is "outputs" within the working directory.
#' @return A `tibble` with measurement id, measurement date, instrument name,
#' level2_quality, set of sensor (irradiance and radiances),
#' waterquality values of TSM, Chla, Kd, and cpc as provided by instrument by
#' default, all the reflectance values from 350 to 900 nm.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data <- wisp_get_reflectance_multi_data(
#'   time_from = "2024-04-08T09:00",
#'   time_to = "2024-04-10T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' 
#' # NA data on 2024-09-01
#' reflect_data <- wisp_get_reflectance_multi_data(
#'   time_from = "2024-08-31T09:00",
#'   time_to = "2024-09-02T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' }
#' ## End (Not run)
#' 
### wisp_get_reflectance_multi_data
wisp_get_reflectance_multi_data <- function(
    version   = "1.0",
    time_from = NULL,
    time_to   = NULL,
    station   = NULL,
    userid    = NULL,
    pwd       = NULL,
    save_csv  = FALSE,
    out_dir   = "outputs"
) {
  
  from_time <- sub(".*T", "", time_from)
  to_time   <- sub(".*T", "", time_to)
  dates <- seq.Date(
    from = as.Date(time_from),
    to   = as.Date(time_to),
    by   = "day"
  )
  
  # skip dates with no data
  daily_list <- lapply(dates, function(date) {
    this_from <- paste0(date, "T", from_time)
    this_to   <- paste0(date, "T", to_time)
    tryCatch({
      wisp_get_reflectance_data(
        version  = version,
        time_from = this_from,
        time_to   = this_to,
        station   = station,
        userid    = userid,
        pwd       = pwd,
        save_csv  = FALSE
      )
    }, error = function(e) {
      message(sprintf(
        "\n----\n⚠️ Skipping %s: %s\n----\n",
        as.character(date),
        e$message
      ))
      return(NULL)
    })
  })
  
  # combine only successful results
  data_multiDates <- dplyr::bind_rows(daily_list)
  
  # remove column "X"
  if ("X" %in% colnames(data_multiDates)) {
    data_multiDates <- dplyr::select(data_multiDates, -X)
  }
  
  # create output csv file
  if (save_csv) {
    date_from <- format(as.Date(time_from), "%Y%m%d")
    date_to   <- format(as.Date(time_to),   "%Y%m%d")
    dates_str <- if (identical(date_from, date_to)) date_from else paste0(date_from, "_", date_to)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    file_path <- file.path(out_dir, paste0("reflectance_data_", dates_str, ".csv"))
    readr::write_csv(x = data_multiDates, file = file_path)
  }
  # output
  return(data_multiDates)
}

#' Quality Control (QC) for WISPstation reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function removes all anomalous spectral signatures and 
#' explains the reason for each elimination
#' @param data A `tibble`. From wisp_get_reflectance_data() function.
#' @param maxPeak A `decimal`. Maximum magnitude of the spectral signatures.
#' We recommend setting this parameter to: 0.02 for clear and oligotrophic water,
#' 0.05 for meso- to eutrophic water, and 0.08 for hypereutrophic and highly turbid water.
#' Default is 0.05.
#' @param maxPeak_blue A `decimal`. Maximum magnitude 350 nm values.
#' We recommend setting this parameter to: 0.02 (default). 
#' @param qa_threshold A `decimal`. Minimum threshold for Quality Assurance (QA).
#' We recommend setting this parameter to: 0.5 (default). 
#' To make QC more stringent, raise the threshold.
#' @param qwip_threshold A `decimal`. Maximum threshold for Quality Water Index Polynomial (QWIP).
#' We recommend setting this parameter to: 0.2 (default). 
#' To make QC more stringent, decrease the threshold.
#' @param calc_scatt A `logical`. If `TRUE`, the function calculates the 
#' peak due to phytoplankton scattering (690-710 nm) and the ratio of the latter 
#' to the second chlorophyll absorption peak (670-680 nm). Default is `TRUE`.
#' @param calc_SPM A `logical`. If `TRUE`, the function calculates the 
#' SPM concentrations in according to Novoa et al., (2017). Default is `TRUE`.
#' @param calc_TUR A `logical`. If `TRUE`, the function calculates the 
#' turbidity (FNU) in according to Novoa et al., (2017). Default is `TRUE`.
#' @param calc_TSS A `logical`. If `TRUE`, the function calculates the 
#' TSS concentrations in according to Jiang et al., (2021). Default is `TRUE`.
#' @param calc_gons A `logical`. If `TRUE`, the function calculates chlorophyll 
#' using Gons et al. (2002) algorithm (NIR ~782 nm). Default is `TRUE`.
#' @param calc_gons740 A `logical`. If `TRUE`, the function calculates chlorophyll 
#' using Gons et al. (2002) algorithm (NIR ~740 nm). Default is `TRUE`.
#' @param calc_NDCI A `logical`. If `TRUE`, the function calculates The Normalised
#' Difference Chlorophyll Index algorithm by Mishra and Mishra (2012). Default is `TRUE`.
#' @param calc_mishra A `logical`. If `TRUE`, the function calculates chlorophyll 
#' using Mishra and Mishra (2012) algorithm. Default is `TRUE`.
#' @param calc_dom_wave A `logical`. If `TRUE`, the function calculates the hue 
#' angle and the dominant wavelength. Default is `TRUE`.
#' @param calc_OWT A `logical`. If `TRUE`, the function calculates the Optical 
#' Water Type classification in according to Bi and Hieronymi (2024) (OWT_class), 
#' the membership probability (OWT_score), and the average Z-score 
#' (OWT_z_dist). In addition, it adds a column with the description of the 
#' corresponding OWT class. "OWT_class" represents the optical category to which 
#' the analyzed spectral signature belongs based on Gaussian Likelihood. 
#' "OWT_score" is a value between 0 and 1 representing the fuzzy membership grade; 
#' it indicates the probability of belonging to the selected class relative to 
#' the other available classes. "OWT_z_dist" indicates the statistical 
#' distance between the observed spectrum and the class mean, weighted by its 
#' standard deviation. 0-1.5 indicate an excellent fit with the OWT reference. 
#' Values above 3 suggest that the spectral signature is an outlier or deviates 
#' significantly from the typical range of that class. Default is `TRUE`.
#' @param save_csv A `logical`. If `TRUE`, the function saves the reflectance data.
#' @param out_dir A `character`. The directory where the CSV file will be saved.
#' Default is "outputs" within the working directory.
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
#' reflect_data_qc <- wisp_qc_reflectance_data(
#'   data = reflect_data,
#'   maxPeak = 0.05,
#'   maxPeak_blue = 0.02,
#'   qa_threshold    = 0.5,
#'   qwip_threshold  = 0.2,
#'   calc_scatt = TRUE,
#'   calc_SPM = TRUE,
#'   calc_TUR = TRUE,
#'   calc_TSS = TRUE,
#'   calc_gons = TRUE,
#'   calc_gons740 = TRUE,
#'   calc_NDCI = TRUE,
#'   calc_mishra = TRUE,
#'   calc_dom_wave = TRUE,
#'   calc_OWT = TRUE,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' }
#' ## End (Not run)
#' 
### wisp_qc_reflectance_data
wisp_qc_reflectance_data <- function(
    data,
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
    out_dir        = "outputs"
) {
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
  
  # QC4 -> Removal lines with outliers in the Blue domain (or Blue > "maxPeak_blue")
  # Find the band closest to 350 nm among the available columns (350-450 nm)
  candidate_bands <- grep("^nm_3[5-9][0-9]$|^nm_350$|^nm_4[0-4][0-9]$|^nm_450$", names(data), value = TRUE)
  if (length(candidate_bands) > 0) {
    band_wavelengths <- as.numeric(sub("nm_", "", candidate_bands))
    closest_idx <- which.min(abs(band_wavelengths - 350))
    blue_ref_band <- candidate_bands[closest_idx]
  } else {
    stop("⚠️ No band between 350 and 450 nm available for QC4.")
  }
  removed_QC4 <- data[which(
    (data[[blue_ref_band]] > pmax(data$nm_555,
                                  data$nm_560,
                                  data$nm_565,
                                  data$nm_570,
                                  data$nm_575) &
       pmax(data$nm_555,
            data$nm_560,
            data$nm_565,
            data$nm_570,
            data$nm_575) > data$nm_495) |
      (data[[blue_ref_band]] > maxPeak_blue)
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
  
  # QA -> Removal of lines according to Wei et al., 2016
  # QA reference data
  QA_ref_csv_text <- paste0(
    "Water_Type,412,443,488,510,531,547,555,667,678\n",
    "median_1,0.738,0.535,0.335,0.169,0.112,0.084,0.072,0.007,0.007\n",
    "median_2,0.677,0.534,0.394,0.225,0.156,0.12,0.104,0.011,0.01\n",
    "median_3,0.608,0.521,0.436,0.28,0.204,0.161,0.14,0.016,0.017\n",
    "median_4,0.51,0.478,0.462,0.348,0.279,0.23,0.206,0.029,0.031\n",
    "median_5,0.43,0.436,0.472,0.386,0.326,0.278,0.253,0.038,0.041\n",
    "median_6,0.363,0.387,0.458,0.408,0.368,0.328,0.304,0.042,0.047\n",
    "median_7,0.309,0.355,0.451,0.419,0.392,0.356,0.335,0.048,0.052\n",
    "median_8,0.276,0.315,0.415,0.415,0.414,0.394,0.378,0.062,0.067\n",
    "median_9,0.349,0.335,0.391,0.386,0.387,0.382,0.378,0.09,0.118\n",
    "median_10,0.228,0.275,0.383,0.407,0.43,0.427,0.42,0.079,0.082\n",
    "median_11,0.291,0.276,0.342,0.367,0.401,0.424,0.437,0.129,0.181\n",
    "median_12,0.187,0.241,0.342,0.382,0.427,0.45,0.461,0.147,0.151\n",
    "median_13,0.173,0.22,0.342,0.393,0.447,0.462,0.464,0.093,0.096\n",
    "median_14,0.188,0.235,0.319,0.363,0.412,0.445,0.463,0.215,0.214\n",
    "median_15,0.143,0.191,0.306,0.365,0.434,0.472,0.492,0.17,0.18\n",
    "median_16,0.181,0.2,0.261,0.307,0.365,0.41,0.437,0.359,0.374\n",
    "median_17,0.174,0.203,0.283,0.334,0.399,0.446,0.472,0.272,0.28\n",
    "median_18,0.142,0.169,0.279,0.349,0.439,0.498,0.525,0.121,0.131\n",
    "median_19,0.05,0.126,0.219,0.277,0.34,0.392,0.423,0.452,0.449\n",
    "median_20,0.117,0.153,0.258,0.324,0.412,0.477,0.515,0.243,0.259\n",
    "median_21,0.163,0.175,0.249,0.308,0.4,0.49,0.544,0.19,0.217\n",
    "median_22,0.111,0.135,0.226,0.292,0.385,0.463,0.511,0.31,0.329\n",
    "median_23,0.145,0.133,0.176,0.215,0.286,0.423,0.548,0.341,0.449\n",
    "lower_1,0.709,0.512,0.271,0.119,0.073,0.053,0.044,0.002,0.002\n",
    "lower_2,0.638,0.509,0.364,0.198,0.132,0.1,0.084,0.003,0.003\n",
    "lower_3,0.553,0.497,0.412,0.246,0.179,0.14,0.119,0.007,0.007\n",
    "lower_4,0.436,0.438,0.419,0.31,0.241,0.193,0.169,0.01,0.011\n",
    "lower_5,0.365,0.39,0.417,0.366,0.287,0.232,0.202,0.016,0.015\n",
    "lower_6,0.307,0.36,0.405,0.387,0.347,0.297,0.272,0.029,0.028\n",
    "lower_7,0.251,0.315,0.415,0.403,0.373,0.334,0.306,0.016,0.021\n",
    "lower_8,0.195,0.266,0.375,0.386,0.39,0.371,0.345,0.023,0.025\n",
    "lower_9,0.295,0.316,0.367,0.362,0.359,0.352,0.341,0.058,0.066\n",
    "lower_10,0.131,0.234,0.336,0.381,0.407,0.39,0.376,0.022,0.032\n",
    "lower_11,0.247,0.24,0.311,0.345,0.366,0.37,0.377,0.085,0.118\n",
    "lower_12,0.148,0.207,0.302,0.336,0.409,0.425,0.427,0.11,0.115\n",
    "lower_13,0.092,0.161,0.313,0.375,0.423,0.438,0.436,0.024,0.023\n",
    "lower_14,0.158,0.2,0.265,0.311,0.382,0.427,0.438,0.154,0.179\n",
    "lower_15,0.066,0.149,0.273,0.334,0.418,0.455,0.466,0.135,0.143\n",
    "lower_16,0.156,0.161,0.226,0.282,0.356,0.394,0.417,0.328,0.332\n",
    "lower_17,0.137,0.176,0.252,0.31,0.388,0.418,0.437,0.244,0.243\n",
    "lower_18,0.058,0.116,0.249,0.321,0.419,0.48,0.499,0.05,0.054\n",
    "lower_19,0.032,0.08,0.183,0.246,0.324,0.378,0.411,0.417,0.409\n",
    "lower_20,0.036,0.096,0.218,0.293,0.395,0.464,0.49,0.204,0.217\n",
    "lower_21,0.107,0.141,0.199,0.246,0.347,0.464,0.508,0.149,0.171\n",
    "lower_22,0.073,0.098,0.2,0.249,0.33,0.45,0.485,0.264,0.292\n",
    "lower_23,0.093,0.095,0.146,0.194,0.265,0.382,0.485,0.301,0.383\n",
    "upper_1,0.78,0.559,0.367,0.203,0.138,0.109,0.096,0.046,0.047\n",
    "upper_2,0.711,0.555,0.424,0.254,0.182,0.141,0.126,0.028,0.027\n",
    "upper_3,0.646,0.54,0.471,0.322,0.243,0.197,0.173,0.067,0.062\n",
    "upper_4,0.57,0.515,0.528,0.374,0.312,0.265,0.24,0.062,0.062\n",
    "upper_5,0.478,0.488,0.548,0.418,0.352,0.314,0.301,0.099,0.098\n",
    "upper_6,0.423,0.416,0.506,0.427,0.39,0.358,0.345,0.065,0.071\n",
    "upper_7,0.362,0.386,0.485,0.439,0.413,0.378,0.36,0.09,0.096\n",
    "upper_8,0.328,0.343,0.464,0.449,0.441,0.418,0.412,0.094,0.14\n",
    "upper_9,0.429,0.369,0.434,0.413,0.412,0.403,0.41,0.166,0.175\n",
    "upper_10,0.283,0.318,0.471,0.451,0.451,0.454,0.452,0.128,0.125\n",
    "upper_11,0.36,0.319,0.373,0.4,0.427,0.451,0.477,0.17,0.284\n",
    "upper_12,0.253,0.287,0.374,0.405,0.439,0.475,0.507,0.183,0.188\n",
    "upper_13,0.235,0.253,0.392,0.424,0.473,0.486,0.488,0.128,0.134\n",
    "upper_14,0.263,0.263,0.35,0.382,0.429,0.461,0.507,0.262,0.276\n",
    "upper_15,0.202,0.219,0.333,0.381,0.448,0.493,0.521,0.203,0.224\n",
    "upper_16,0.23,0.224,0.296,0.339,0.382,0.432,0.465,0.393,0.419\n",
    "upper_17,0.232,0.244,0.316,0.355,0.415,0.463,0.503,0.302,0.313\n",
    "upper_18,0.202,0.204,0.309,0.376,0.455,0.522,0.56,0.163,0.17\n",
    "upper_19,0.066,0.147,0.236,0.296,0.367,0.415,0.439,0.479,0.493\n",
    "upper_20,0.159,0.184,0.296,0.356,0.429,0.5,0.571,0.29,0.293\n",
    "upper_21,0.235,0.237,0.293,0.336,0.443,0.515,0.605,0.241,0.286\n",
    "upper_22,0.159,0.167,0.251,0.318,0.408,0.482,0.573,0.351,0.383\n",
    "upper_23,0.18,0.167,0.198,0.233,0.31,0.452,0.578,0.379,0.509\n"
  )  
  
  QA_dt <- read.csv(text = QA_ref_csv_text, header = TRUE, row.names = 1)
  QA_dt[] <- lapply(QA_dt, as.numeric)
  QA_median <- QA_dt[1:23, ]
  QA_lower <- QA_dt[24:46, ]
  QA_upper <- QA_dt[47:69, ]
  target_waves <- c(412, 443, 488, 510, 531, 547, 555, 667, 678)
  
  mean_band <- function(row, wave, window = 3) {
    band_names <- paste0("nm_", (wave - window):(wave + window))
    band_names <- band_names[band_names %in% names(row)]
    mean(as.numeric(row[band_names]), na.rm = TRUE)
  }
  
  cosa <- function(tmp_nRrs, one_median) {
    sum(tmp_nRrs * one_median) / sqrt(sum(tmp_nRrs^2) * sum(one_median^2))
  }
  
  QA_wei <- function(one_Rrs, QA_median, QA_lower, QA_upper) {
    nRrs <- one_Rrs / sqrt(sum(one_Rrs^2))
    all_cosa <- apply(QA_median, 1, cosa, tmp_nRrs = nRrs)
    max_cosa <- which.max(all_cosa)
    found_lower <- QA_lower[max_cosa, ]
    found_upper <- QA_upper[max_cosa, ]
    tmp_QA <- as.numeric(nRrs >= found_lower & nRrs <= found_upper)
    mean(tmp_QA)
  }
  
  spec_cols  <- grep("^nm_", colnames(data), value = TRUE)
  spec_waves <- as.numeric(sub("nm_", "", spec_cols))
  QA_input_idx <- which(removed_rows$reason == "")
  QA_score <- rep(NA, initial_nrow)
  for (i in QA_input_idx) {
    row_list <- as.list(data[i, ])
    band_means <- sapply(target_waves, function(w) mean_band(row_list, w))
    if (any(is.na(band_means))) {
      QA_score[i] <- NA
    } else {
      raw_QA_score <- QA_wei(band_means, QA_median, QA_lower, QA_upper)
      QA_score[i] <- round(raw_QA_score, 2)
    }
  }
  
  removed_QA <- which(QA_score < qa_threshold | is.na(QA_score))
  removed_rows$reason[removed_QA] <-
    paste(trimws(removed_rows$reason[removed_QA]), "QA")
  
  # QWIP -> Removal of lines according to Dierssen et al., 2022
  cal_QWIP_score_hyper <- function(rrs, wave){
    rrs <- as.numeric(rrs)
    names(rrs) <- paste0("Rrs", wave)
    vis_dt <- data.frame(wl=seq(400,700),
                         Rrs=rrs[which(wave==400):which(wave==700)])
    AVW <- sum(vis_dt$Rrs,na.rm=TRUE)/sum(vis_dt$Rrs/vis_dt$wl,na.rm=TRUE)
    NDI <- (rrs["Rrs665"]-rrs["Rrs492"])/(rrs["Rrs665"]+rrs["Rrs492"])
    p <- c(-8.399885e-9,1.715532e-5,-1.301670e-2,4.357838e0,-5.449532e2)
    QWIP_exp <- p[1]*AVW^4 + p[2]*AVW^3 + p[3]*AVW^2 + p[4]*AVW + p[5]
    NDI - QWIP_exp
  }
  
  QWIP_score <- rep(NA, initial_nrow)
  QWIP_input_idx <- which(removed_rows$reason == "")
  for (i in QWIP_input_idx) {
    raw_QWIP_score <- cal_QWIP_score_hyper(
      as.numeric(data[i, spec_cols]), spec_waves
    )
    QWIP_score[i] <- round(raw_QWIP_score, 4)
  }
  
  removed_QWIP <- which(QWIP_score > qwip_threshold | is.na(QWIP_score))
  removed_rows$reason[removed_QWIP] <-
    paste(trimws(removed_rows$reason[removed_QWIP]), "QWIP")
  
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
    QC6 = "remove 'invalid' and 'None' spectral signatures according to level2.quality",
    QA = "remove spectral signatures with low quality based on Wei et al. (2016)",
    QWIP = "remove spectral signatures with low quality based on Dierssen et al., 2022"
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
    message("\n----\n⚠️ Thank you for your request, but the QC operation removed all the spectral signatures available on this date.\n----\n")
    return(NULL)
  }
  
  final_idx <- which(removed_rows$reason == "")
  reflectance_data_filtered <- data[final_idx, ]
  
  reflectance_data_filtered$QA_score   <- QA_score[final_idx]
  reflectance_data_filtered$QWIP_score <- QWIP_score[final_idx]
  
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("nm_"), ~ units::set_units(as.numeric(as.character(.)), "1/sr")
      ),
      QC = "checked"
    )
  
  # for indexes
  if (calc_scatt) {
    reflectance_data_filtered <- wisp_calc_scatt(reflectance_data_filtered)
  }
  if (calc_SPM) {
    reflectance_data_filtered <- wisp_calc_Novoa_SPM(reflectance_data_filtered)
  }
  if (calc_TUR) {
    reflectance_data_filtered <- wisp_calc_Novoa_TUR(reflectance_data_filtered)
    after_col <- if (calc_SPM) "Blended.SPM" else "waterquality.tsm"
    reflectance_data_filtered <- dplyr::relocate(
      reflectance_data_filtered,
      Novoa.TUR, Blended.TUR,
      .after = all_of(after_col)
    )
  }
  if (calc_TSS) {
    reflectance_data_filtered <- wisp_calc_Jiang_TSS(reflectance_data_filtered)
    after_col <- if (!calc_SPM && !calc_TUR) {
      "waterquality.tsm"
    } else if (calc_SPM && !calc_TUR) {
      "Blended.SPM"
    } else {
      "Blended.TUR"
    }
    reflectance_data_filtered <- dplyr::relocate(
      reflectance_data_filtered,
      Jiang.TSS, ref_band, a, bbp,
      .after = all_of(after_col)
    )
  }
  if (calc_gons) {
    reflectance_data_filtered <- wisp_calc_Gons_CHL(reflectance_data_filtered)
    if (all(c("Gons.CHL", "waterquality.chla") %in% names(reflectance_data_filtered))) {
      reflectance_data_filtered <- dplyr::relocate(
        reflectance_data_filtered,
        Gons.CHL,
        .after = all_of("waterquality.chla")
      )
    }
  }
  if (calc_gons740) {
    reflectance_data_filtered <- wisp_calc_Gons740_CHL(reflectance_data_filtered)
    if ("Gons740.CHL" %in% names(reflectance_data_filtered)) {
      after_col <- if ("Gons.CHL" %in% names(reflectance_data_filtered)) "Gons.CHL" else "waterquality.chla"
      reflectance_data_filtered <- dplyr::relocate(
        reflectance_data_filtered,
        Gons740.CHL,
        .after = all_of(after_col)
      )
    }
  }
  if (calc_NDCI) {
    reflectance_data_filtered <- wisp_calc_NDCI(reflectance_data_filtered)
    after_col <- if (!("Gons.CHL" %in% names(reflectance_data_filtered)) &&
                     !("Gons740.CHL" %in% names(reflectance_data_filtered))) {
      "waterquality.chla"
    } else if (!("Gons740.CHL" %in% names(reflectance_data_filtered))) {
      "Gons.CHL"
    } else {
      "Gons740.CHL"
    }
    reflectance_data_filtered <- dplyr::relocate(
      reflectance_data_filtered,
      NDCI,
      .after = all_of(after_col)
    )
  }
  if (calc_mishra) {
    reflectance_data_filtered <- wisp_calc_Mishra_CHL(reflectance_data_filtered)
    after_col <- "NDCI"
    reflectance_data_filtered <- dplyr::relocate(
      reflectance_data_filtered,
      Mishra.CHL,
      .after = all_of(after_col)
    )
  }
  if (calc_dom_wave) {
    reflectance_data_filtered <- wisp_calc_dom_wave(reflectance_data_filtered)
    reflectance_data_filtered <- dplyr::relocate(
      reflectance_data_filtered,
      hue_angle, dominant_wavelength,
      .after = all_of("waterquality.cpc")
    )
  }
  if (calc_OWT) {
    reflectance_data_filtered <- wisp_calc_OWT_class(reflectance_data_filtered)
    reflectance_data_filtered <- reflectance_data_filtered |> 
      dplyr::relocate(OWT_class, OWT_description, OWT_score, OWT_z_dist, .after = QWIP_score)
  }
  
  # create output csv file
  if (save_csv) {
    date_from <- format(as.Date(reflectance_data_filtered$measurement.date[1]), "%Y%m%d")
    date_to <- format(as.Date(reflectance_data_filtered$measurement.date[nrow(reflectance_data_filtered)]), "%Y%m%d")
    dates <- if (identical(date_from, date_to)) date_from else paste0(date_from, "_", date_to)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    file <- file.path(out_dir, paste0("reflectance_data", "_qc_", dates, ".csv"))
    readr::write_csv(x = reflectance_data_filtered, file = file)
  }
  
  return(reflectance_data_filtered)
}

#' SUNGLINT Removal (SR) for WISPstation reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function applies the algorithm of Jiang et al. (2020) for removing
#' sunglint from spectral signatures. It calculates an offset value, 
#' represented by the **delta** column, based on the Relative Height Water (RHW) 
#' in the NIR region to correct the surface reflectance.
#' @param qc_data A `tibble` from wisp_qc_reflectance_data() function.
#' @param calc_scatt A `logical`. If `TRUE`, the function calculates the 
#' peak due to phytoplankton scattering (690-710 nm) and the ratio of the latter 
#' to the second chlorophyll absorption peak (670-680 nm). Default is `TRUE`.
#' @param calc_SPM A `logical`. If `TRUE`, the function calculates the 
#' SPM concentrations in according to Novoa et al., (2017). Default is `TRUE`.
#' @param calc_TUR A `logical`. If `TRUE`, the function calculates the 
#' turbidity (FNU) in according to Novoa et al., (2017). Default is `TRUE`.
#' @param calc_TSS A `logical`. If `TRUE`, the function calculates the 
#' TSS concentrations in according to Jiang et al., (2021). Default is `TRUE`.
#' @param calc_gons A `logical`. If `TRUE`, the function calculates chlorophyll 
#' using Gons et al. (2002) algorithm (NIR ~782 nm). Default is `TRUE`.
#' @param calc_gons740 A `logical`. If `TRUE`, the function calculates chlorophyll 
#' using Gons et al. (2002) algorithm (NIR ~740 nm). Default is `TRUE`.
#' @param calc_NDCI A `logical`. If `TRUE`, the function calculates The Normalised
#' Difference Chlorophyll Index algorithm by Mishra and Mishra (2012). Default is `TRUE`.
#' @param calc_mishra A `logical`. If `TRUE`, the function calculates chlorophyll 
#' using Mishra and Mishra (2012) algorithm. Default is `TRUE`.
#' @param calc_dom_wave A `logical`. If `TRUE`, the function calculates the hue 
#' angle and the dominant wavelength. Default is `TRUE`.
#' @param calc_OWT A `logical`. If `TRUE`, the function calculates the Optical 
#' Water Type classification in according to Bi and Hieronymi (2024) (OWT_class), 
#' the membership probability (OWT_score), and the average Z-score 
#' (OWT_z_dist). In addition, it adds a column with the description of the 
#' corresponding OWT class. "OWT_class" represents the optical category to which 
#' the analyzed spectral signature belongs based on Gaussian Likelihood. 
#' "OWT_score" is a value between 0 and 1 representing the fuzzy membership grade; 
#' it indicates the probability of belonging to the selected class relative to 
#' the other available classes. "OWT_z_dist" indicates the statistical 
#' distance between the observed spectrum and the class mean, weighted by its 
#' standard deviation. 0-1.5 indicate an excellent fit with the OWT reference. 
#' Values above 3 suggest that the spectral signature is an outlier or deviates 
#' significantly from the typical range of that class. Default is `TRUE`.
#' @param save_csv A `logical`. If `TRUE`, the function saves the reflectance data.
#' @param out_dir A `character`. The directory where the CSV file will be saved.
#' Default is "outputs" within the working directory.
#' @return A tibble with the spectral signatures after the SR operation and,
#' if parameter `save_out_sr` is `TRUE`, the function saves the reflectance data
#' in a CSV file.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate select all_of across
#' @importFrom units set_units
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data_sr <- wisp_sr_reflectance_data(
#'   qc_data = reflect_data_qc,
#'   calc_scatt = TRUE,
#'   calc_SPM = TRUE,
#'   calc_TUR = TRUE,
#'   calc_TSS = TRUE,
#'   calc_gons  = TRUE,
#'   calc_gons740 = TRUE,
#'   calc_NDCI = TRUE,
#'   calc_mishra = FALSE,
#'   calc_dom_wave = TRUE,
#'   calc_OWT = TRUE,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' }
#' ## End (Not run)
#'
### wisp_sr_reflectance_data
wisp_sr_reflectance_data <- function(
    qc_data,
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
    out_dir       = "outputs"
) {
  if (!"QC" %in% names(qc_data)) {
    message("\n----\n⚠️ This function is not executable on this dataset. Try after QC.\n----\n")
    return(invisible(NULL))
  }
  
  columns_nm       <- grep("^nm_", colnames(qc_data), value = TRUE)
  
  # ranges for SR checks
  cols_350_400     <- grep("^nm_3(5[0-9]|6[0-9]|7[0-9]|8[0-9]|9[0-9]|400)$", columns_nm, value = TRUE)
  cols_400_700     <- grep("^nm_([4-6][0-9]{2}|700)$", columns_nm, value = TRUE)
  
  # deglint algorithm
  tmp <- dplyr::mutate(
    qc_data,
    md_750_780 = apply(dplyr::select(qc_data, matches("^nm_(7[5-9][0-9]|780)$")), 1, median, na.rm = TRUE),
    md_780     = apply(dplyr::select(qc_data, matches("^nm_(77[5-9]|78[0-5])$")), 1, median, na.rm = TRUE),
    md_810     = apply(dplyr::select(qc_data, matches("^nm_(80[5-9]|81[0-5])$")), 1, median, na.rm = TRUE),
    md_840     = apply(dplyr::select(qc_data, matches("^nm_(83[5-9]|84[0-5])$")), 1, median, na.rm = TRUE),
    RHW        = md_810 - md_780 - ((md_840 - md_780) * (810 - 780) / (840 - 780)),
    est_md_750_780 = 18267.884 * RHW^3 - 129.158 * RHW^2 + 3.072 * RHW,
    delta      = units::set_units(ifelse(RHW > 0, md_750_780 - est_md_750_780, md_750_780), "1/sr")
  )
  
  # temporary SR correction
  tmp_corrected <- dplyr::mutate(
    tmp,
    !!!rlang::set_names(
      lapply(columns_nm, function(col) {
        rlang::quo(!!rlang::sym(col) - delta)
      }),
      paste0("check_", columns_nm)
    )
  )
  valid_flag <- apply(tmp_corrected[, paste0("check_", columns_nm)], 1, function(row) {
    vals_350_400 <- row[paste0("check_", cols_350_400)]
    vals_400_700 <- row[paste0("check_", cols_400_700)]
    all(vals_400_700 >= 0, na.rm = TRUE) && all(vals_350_400 <= 0.05, na.rm = TRUE)
  })
  
  # definitive SR correction
  corrected_data <- dplyr::mutate(
    tmp,
    SR = ifelse(valid_flag, "corrected", "not corrected (risk of overcorrection)")
  )
  
  # apply SR adjustment
  for (col in columns_nm) {
    corrected_data[[col]] <- ifelse(valid_flag, corrected_data[[col]] - corrected_data$delta, corrected_data[[col]])
  }
  
  # set delta to NA for uncorrected rows
  corrected_data <- dplyr::mutate(
    corrected_data,
    delta = ifelse(
      SR == "not corrected (risk of overcorrection)",
      units::set_units(NA_real_, "1/sr"),
      delta
    )
  )
  
  # remove intermediate columns
  corrected_data <- dplyr::select(
    corrected_data,
    -starts_with("check_"),
    -md_750_780, -md_780, -md_810, -md_840,
    -RHW, -est_md_750_780
  )
  
  # remove columns calculated before
  indices_to_remove <- c(
    "scattering.peak", "band.ratio",           
    "Novoa.SPM", "Blended.SPM",              
    "Novoa.TUR", "Blended.TUR",               
    "Jiang.TSS", "ref_band", "a", "bbp",      
    "Gons.CHL", "Gons740.CHL",                
    "NDCI", "Mishra.CHL",                     
    "hue_angle", "dominant_wavelength",
    "OWT_class", "OWT_score", "OWT_z_dist"
  )
  corrected_data <- corrected_data |>
    dplyr::select(-dplyr::any_of(indices_to_remove))
  
  # for indexes
  if (calc_scatt) {
    corrected_data <- wisp_calc_scatt(corrected_data)
  }
  if (calc_SPM) {
    corrected_data <- wisp_calc_Novoa_SPM(corrected_data)
  }
  if (calc_TUR) {
    corrected_data <- wisp_calc_Novoa_TUR(corrected_data)
    after_col <- if (calc_SPM) "Blended.SPM" else "waterquality.tsm"
    corrected_data <- dplyr::relocate(
      corrected_data,
      Novoa.TUR, Blended.TUR,
      .after = all_of(after_col)
    )
  }
  if (calc_TSS) {
    corrected_data <- wisp_calc_Jiang_TSS(corrected_data)
    after_col <- if (!calc_SPM && !calc_TUR) {
      "waterquality.tsm"
    } else if (calc_SPM && !calc_TUR) {
      "Blended.SPM"
    } else {
      "Blended.TUR"
    }
    corrected_data <- dplyr::relocate(
      corrected_data,
      Jiang.TSS, ref_band, a, bbp,
      .after = all_of(after_col)
    )
  }
  if (calc_gons) {
    corrected_data <- wisp_calc_Gons_CHL(corrected_data)
    if (all(c("Gons.CHL", "waterquality.chla") %in% names(corrected_data))) {
      corrected_data <- dplyr::relocate(
        corrected_data,
        Gons.CHL,
        .after = all_of("waterquality.chla")
      )
    }
  }
  if (calc_gons740) {
    corrected_data <- wisp_calc_Gons740_CHL(corrected_data)
    if ("Gons740.CHL" %in% names(corrected_data)) {
      after_col <- if ("Gons.CHL" %in% names(corrected_data)) "Gons.CHL" else "waterquality.chla"
      corrected_data <- dplyr::relocate(
        corrected_data,
        Gons740.CHL,
        .after = all_of(after_col)
      )
    }
  }
  if (calc_NDCI) {
    corrected_data <- wisp_calc_NDCI(corrected_data)
    after_col <- if (!("Gons.CHL" %in% names(corrected_data)) &&
                     !("Gons740.CHL" %in% names(corrected_data))) {
      "waterquality.chla"
    } else if (!("Gons740.CHL" %in% names(corrected_data))) {
      "Gons.CHL"
    } else {
      "Gons740.CHL"
    }
    corrected_data <- dplyr::relocate(
      corrected_data,
      NDCI,
      .after = all_of(after_col)
    )
  }
  if (calc_mishra) {
    corrected_data <- wisp_calc_Mishra_CHL(corrected_data)
    after_col <- "NDCI"
    corrected_data <- dplyr::relocate(
      corrected_data,
      Mishra.CHL,
      .after = all_of(after_col)
    )
  }
  if (calc_dom_wave) {
    corrected_data <- wisp_calc_dom_wave(corrected_data)
    corrected_data <- dplyr::relocate(
      corrected_data,
      hue_angle, dominant_wavelength,
      .after = all_of("waterquality.cpc")
    )
  }
  if (calc_OWT) {
    corrected_data <- wisp_calc_OWT_class(corrected_data)
    corrected_data <- corrected_data |> 
      dplyr::relocate(OWT_class, OWT_description, OWT_score, OWT_z_dist, .after = QWIP_score)
  }
  
  # create output csv file
  if (save_csv) {
    date_from <- format(as.Date(corrected_data$measurement.date[1]), "%Y%m%d")
    date_to   <- format(as.Date(corrected_data$measurement.date[nrow(corrected_data)]), "%Y%m%d")
    dates <- if (identical(date_from, date_to)) date_from else paste0(date_from, "_", date_to)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    file <- file.path(out_dir, paste0("reflectance_data", "_sr_", dates, ".csv"))
    readr::write_csv(x = corrected_data, file = file)
  }
  
  # Output
  return(corrected_data)
}



#' @noRd
#' @keywords internal
### wisp_calc_scatt
wisp_calc_scatt <- function(data) {
  columns_690_710 <- grep("^nm_(69[0-9]|70[0-9]|710)$", colnames(data), value = TRUE)
  columns_670_680 <- grep("^nm_(67[0-9]|680)$", colnames(data), value = TRUE)
  
  data <- data |>
    dplyr::mutate(
      scattering.peak = units::set_units(
        round(as.numeric(apply(dplyr::select(data, dplyr::all_of(columns_690_710)), 1, max, na.rm = TRUE)), 4),
        "1/sr"
      ),
      band.ratio = units::set_units(
        round(scattering.peak / apply(dplyr::select(data, dplyr::all_of(columns_670_680)), 1, max, na.rm = TRUE), 4),
        "1"
      )
    ) |>
    dplyr::relocate(scattering.peak, band.ratio, .after = waterquality.chla)
}

#' @noRd
#' @keywords internal
### wisp_calc_Novoa_SPM
wisp_calc_Novoa_SPM <- function(data) {
  
  # General parameters
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
      stop("⚠️ Wavelength not supported")
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
    coeff_red <- get_nechad_spm_coefficients(665)
    coeff_nir <- get_nechad_spm_coefficients(865)
    
    spm_green <- calculate_spm(green_value, coeff_green$A, coeff_green$C)
    spm_red <- calculate_spm(red_value, coeff_red$A, coeff_red$C)
    spm_nir <- calculate_spm(nir_value, coeff_nir$A, coeff_nir$C)
    
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
    } else if (red_value <= rhow_nir_lower) {
      spm_final <- spm_red
      band_selected <- "Red (665nm)"
    } else if (red_value >= rhow_nir_upper) {
      spm_final <- spm_nir
      band_selected <- "NIR (865nm)"
    } else {
      logb <- log(rhow_nir_upper / rhow_nir_lower)
      spm_final <- spm_red * log(rhow_nir_upper / red_value) / logb +
        spm_nir * log(red_value / rhow_nir_lower) / logb
      band_selected <- "Blending red and NIR"
    }
    
    return(list(SPM = spm_final, band_selected = band_selected))
  }
  
  # --- SPM calculation for "data" ---
  green_cols <- get_columns_in_range(data, 560, tol = 3)
  red_cols <- get_columns_in_range(data, 665, tol = 3)
  nir_cols <- get_columns_in_range(data, 865, tol = 3)
  
  data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      green_value = as.numeric(mean(dplyr::c_across(all_of(green_cols)), na.rm = TRUE) * pi),
      red_value = as.numeric(mean(dplyr::c_across(all_of(red_cols)), na.rm = TRUE) * pi),
      nir_value = as.numeric(mean(dplyr::c_across(all_of(nir_cols)), na.rm = TRUE) * pi),
      novoa_spm = list(compute_novoa_spm(green_value, red_value, nir_value))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Novoa.SPM = units::set_units(round(purrr::map_dbl(novoa_spm, "SPM"), 1), "g/m3"),
      Blended.SPM = purrr::map_chr(novoa_spm, "band_selected")
    ) |>
    dplyr::relocate(Novoa.SPM, Blended.SPM, .after = waterquality.tsm) |>
    dplyr::select(-green_value, -red_value, -nir_value, -novoa_spm)
}

#' @noRd
#' @keywords internal
### wisp_calc_Novoa_TUR
wisp_calc_Novoa_TUR <- function(data) {
  
  # General parameters
  novoa_tur_dict <- list(
    novoa_waves_req = c(665, 865),          
    novoa_algorithm = "nechad_centre",      # "nechad_centre" or "nechad_average"
    novoa_output_switch = FALSE,
    rhow_switch_nir = c(0.08, 0.12)         #  or "c(0.046,0.09)" (Bourgneuf) 
  )
  
  # A_Nechad and C_Nechad coefficients (TUR) for green, red and nir bands
  get_nechad_tur_coefficients <- function(wave) {
    if (wave == 665) {
      return(list(A = 282.95, C = 0.1728))
    } else if (wave == 865) {
      return(list(A = 2109.35, C = 0.2115))
    } else {
      stop("⚠️ Wavelength not supported")
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
  
  # --- TUR calculation for "data" ---
  red_cols <- get_columns_in_range(data, 665, tol = 3)
  nir_cols <- get_columns_in_range(data, 865, tol = 3)
  
  units::install_unit(
    symbol = "NTU",
    name = c("Nephelometric Turbidity Unit")
  )
  
  data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      red_value = as.numeric(mean(dplyr::c_across(all_of(red_cols)), na.rm = TRUE) * pi),
      nir_value = as.numeric(mean(dplyr::c_across(all_of(nir_cols)), na.rm = TRUE) * pi),
      novoa_tur = list(compute_novoa_tur(red_value, nir_value))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Novoa.TUR = units::set_units(round(purrr::map_dbl(novoa_tur, "TUR"), 1), "NTU"),
      Blended.TUR = purrr::map_chr(novoa_tur, "band_selected")
    ) |>
    dplyr::select(-red_value, -nir_value, -novoa_tur)
  
  units::remove_unit("NTU", "Nephelometric Turbidity Unit")
  data <- data
}

#' @noRd
#' @keywords internal
### wisp_calc_Jiang_TSS 
wisp_calc_Jiang_TSS <- function(data) {
  
  # Remove any remaining columns from previous calculation
  cols_to_drop <- intersect(names(data), c("a", "bbp", "ref_band", "Jiang.TSS"))
  if (length(cols_to_drop) > 0) {
    data <- data[, !names(data) %in% cols_to_drop]
  }
  
  # General parameters
  wave <- c(443, 490, 560, 620, 665, 754, 865)
  aw <- c(0.005046443, 0.013589323, 0.062122106, 0.276193682, 0.42748488, 2.868335728, 4.639441062)
  bbw <- c(0.00214135, 0.001381358, 0.000778527, 0.000502851, 0.000372427, 0.000217139, 0.000120218)
  names(aw) <- names(bbw) <- paste0("Rrs", wave)
  
  # Single-spectrum calculation
  calc_tss_single <- function(rrs) {
    if (all(is.na(rrs)) || all(rrs == 0)) {
      return(data.frame(a = NA, bbp = NA, ref_band = NA, Jiang.TSS = NA))
    }
    rrs_sub <- rrs / (0.52 + 1.7 * rrs)
    u <- (-0.089 + sqrt(0.089^2 + 4 * 0.125 * rrs_sub)) / (2 * 0.125)
    
    # Band selection
    band <- if (rrs["Rrs490"] > rrs["Rrs560"]) {
      "Rrs560"
    } else if (rrs["Rrs490"] > rrs["Rrs620"]) {
      "Rrs665"
    } else if (rrs["Rrs754"] > rrs["Rrs490"] && rrs["Rrs754"] > 0.010) {
      "Rrs865"
    } else {
      "Rrs754"
    }
    
    # Absorption
    if (band == "Rrs560") {
      x <- log10((rrs["Rrs443"] + rrs["Rrs490"]) /
                   (rrs["Rrs560"] + 5 * (rrs["Rrs665"]^2 / rrs["Rrs490"])))
      a_val_raw <- aw[band] + 10^(-1.146 - 1.366 * x - 0.469 * x^2)
    } else if (band == "Rrs665") {
      a_val_raw <- aw[band] + 0.39 * (rrs["Rrs665"] / (rrs["Rrs443"] + rrs["Rrs490"]))^1.14
    } else {
      a_val_raw <- aw[band]
    }
    
    # Backscattering
    bbp_val_raw <- ((u[band] * a_val_raw) / (1 - u[band])) - bbw[band]
    
    # Round + Units
    a_val   <- units::set_units(round(a_val_raw, 3), "m-1")
    bbp_val <- units::set_units(round(bbp_val_raw, 3), "m-1")
    coeff   <- switch(band, Rrs560=94.6074, Rrs665=114.0121, Rrs754=137.6652, Rrs865=166.1682)
    TSS_val <- units::set_units(round(coeff * bbp_val_raw, 1), "g/m3")
    
    return(data.frame(a = a_val, bbp = bbp_val, ref_band = paste0("QAA_", sub("Rrs", "", band)), Jiang.TSS = TSS_val))
  }
  
  # Compute Rrs means (±3 nm)
  nm_cols <- grep("^nm_", names(data), value = TRUE)
  wl <- as.numeric(sub("^nm_", "", nm_cols))
  cols_list <- lapply(wave, function(w) {
    nm_cols[wl >= w - 3 & wl <= w + 3]
  })
  names(cols_list) <- paste0("Rrs", wave)
  rrs_df <- as.data.frame(
    sapply(cols_list, function(cols) {
      rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
    })
  )
  colnames(rrs_df) <- names(cols_list)
  
  # TSS calculation for each spectral signature
  tss_list <- apply(rrs_df, 1, calc_tss_single)
  tss_df <- dplyr::bind_rows(tss_list)
  
  # Combine with original data
  out <- dplyr::bind_cols(data, tss_df)
  return(out)
}

#' @noRd
#' @keywords internal
### wisp_calc_Gons_CHL 
wisp_calc_Gons_CHL <- function(data) {
  
  # Identification of wavelengths (+/- 3 nm)
  nm_cols <- grep("^nm_", names(data), value = TRUE)
  wl <- as.numeric(sub("^nm_", "", nm_cols))
  get_cols <- function(center, tol = 3) nm_cols[wl >= center - tol & wl <= center + tol]
  
  red_cols <- get_cols(664)
  re_cols  <- get_cols(704)
  nir_cols <- get_cols(782)
  
  # If bands are missing, return NA
  if (length(red_cols) == 0 || length(re_cols) == 0 || length(nir_cols) == 0) {
    warning("⚠️ wisp_calc_Gons_CHL: missing required bands (664/704/782 ±3 nm).")
    data$Gons.CHL <- units::set_units(rep(NA_real_, nrow(data)), "mg/m3")
    return(data)
  }
  
  # Consider Rrs as dimensionless numerical values
  mnum <- function(df) rowMeans(as.data.frame(lapply(df, function(x) as.numeric(x))), na.rm = TRUE)
  rrs_red <- mnum(data[red_cols])
  rrs_re  <- mnum(data[re_cols])
  rrs_nir <- mnum(data[nir_cols])
  
  # Coefficients derived from Gons et al. (2002)
  gc <- c(1.61, 0.082, 0.6, 0.7, 0.40, 1.05)
  astar <- 0.015
  
  denom <- gc[2] - gc[3] * rrs_nir
  bb <- rep(NA_real_, length(rrs_nir))
  ok  <- !is.na(denom) & denom != 0
  bb[ok] <- (gc[1] * rrs_nir[ok]) / denom[ok]
  
  rm <- rrs_re / rrs_red
  
  # CHL concentration algorithm (Gons et al., 2002)
  chl <- ((rm * (gc[4] + bb)) - gc[5] - (bb ^ gc[6])) / astar
  
  # Validation (gm = c(0.005, 0.63)); invalid if (Rrs_red <= 0.005) or (rm <= 0.63)
  gm <- c(0.005, 0.63)
  chl[chl < 0] <- NA_real_
  chl[(rrs_red <= gm[1]) | (rm <= gm[2])] <- NA_real_
  
  # Enter unit of measurement
  data$Gons.CHL <- units::set_units(round(chl, 1), "mg/m3")
  data
}

#' @noRd
#' @keywords internal
### wisp_calc_Gons740_CHL 
wisp_calc_Gons740_CHL <- function(data) {
  
  # Identification of wavelengths (+/- 3 nm)
  nm_cols <- grep("^nm_", names(data), value = TRUE)
  wl <- as.numeric(sub("^nm_", "", nm_cols))
  get_cols <- function(center, tol = 3) nm_cols[wl >= center - tol & wl <= center + tol]
  
  red_cols <- get_cols(664)
  re_cols  <- get_cols(704)
  nir_cols <- get_cols(740)
  
  # If bands are missing, return NA
  if (length(red_cols) == 0 || length(re_cols) == 0 || length(nir_cols) == 0) {
    warning("⚠️ wisp_calc_Gons740_CHL: missing required bands (664/704/740 ±3 nm).")
    data$Gons740.CHL <- units::set_units(rep(NA_real_, nrow(data)), "mg/m3")
    return(data)
  }
  
  # Consider Rrs as dimensionless numerical values
  mnum <- function(df) rowMeans(as.data.frame(lapply(df, function(x) as.numeric(x))), na.rm = TRUE)
  rrs_red <- mnum(data[red_cols])
  rrs_re  <- mnum(data[re_cols])
  rrs_nir <- mnum(data[nir_cols])
  
  # Coefficients derived from Gons et al. (2002)
  gc <- c(1.61, 0.082, 0.6, 0.7, 0.40, 1.05)
  astar <- 0.015
  
  denom <- gc[2] - gc[3] * rrs_nir
  bb <- rep(NA_real_, length(rrs_nir))
  ok  <- !is.na(denom) & denom != 0
  bb[ok] <- (gc[1] * rrs_nir[ok]) / denom[ok]
  
  rm <- rrs_re / rrs_red
  
  # CHL concentration algorithm (Gons et al., 2002)
  chl <- ((rm * (gc[4] + bb)) - gc[5] - (bb ^ gc[6])) / astar
  
  # Validation (gm = c(0.005, 0.63)); invalid if (Rrs_red <= 0.005) or (rm <= 0.63)
  gm <- c(0.005, 0.63)
  chl[chl < 0] <- NA_real_
  chl[(rrs_red <= gm[1]) | (rm <= gm[2])] <- NA_real_
  
  # Set unit of measurement
  data$Gons740.CHL <- units::set_units(round(chl, 1), "mg/m3")
  data
}

#' @noRd
#' @keywords internal
### wisp_calc_NDCI
wisp_calc_NDCI <- function(data) {
  
  # Identification of wavelengths (+/- 3 nm)
  nm_cols <- grep("^nm_", names(data), value = TRUE)
  wl <- as.numeric(sub("^nm_", "", nm_cols))
  get_cols <- function(center, tol = 3) nm_cols[wl >= center - tol & wl <= center + tol]
  
  cols_670 <- get_cols(670)
  cols_705 <- get_cols(705)
  
  # If bands are missing, return NA
  if (length(cols_670) == 0 || length(cols_705) == 0) {
    warning("⚠️ wisp_calc_NDCI: missing required bands (670/705 ±3 nm).")
    data$NDCI <- units::set_units(rep(NA_real_, nrow(data)), "1")
    return(data)
  }
  
  # Consider Rrs as dimensionless numerical values
  mnum <- function(df) rowMeans(as.data.frame(lapply(df, function(x) as.numeric(x))), na.rm = TRUE)
  rrs_670 <- mnum(data[cols_670])
  rrs_705 <- mnum(data[cols_705])
  
  # Compute NDCI, handling division by zero / NA
  numerator <- rrs_705 - rrs_670
  denom <- rrs_705 + rrs_670
  
  ndci <- rep(NA_real_, length(numerator))
  ok <- !is.na(denom) & denom != 0
  ndci[ok] <- numerator[ok] / denom[ok]
  
  # Set unit and rounding consistent with other functions
  data$NDCI <- units::set_units(round(ndci, 4), "1")
  data
}

#' @noRd
#' @keywords internal
### wisp_calc_Mishra_CHL
wisp_calc_Mishra_CHL <- function(data) {
  
  # Check if NDCI column is present
  if (!"NDCI" %in% names(data)) {
    stop("⚠️ Error: the ‘NDCI’ column is not present. First calculate NDCI using 'wisp_calc_NDCI' function.")
  }
  
  # Coefficients derived from Mishra and Mishra (2012)
  a0 <- 14.039
  a1 <- 86.115
  a2 <- 194.325
  
  # Use the NDCI value calculated previously
  ndci <- as.numeric(data$NDCI)
  
  # CHL concentration algorithm (Mishra and Mishra, 2012)
  chl <- rep(NA_real_, length(ndci))
  valid_ndci <- !is.na(ndci)
  chl[valid_ndci] <- a0 + a1 * ndci[valid_ndci] + a2 * (ndci[valid_ndci]^2)
  
  # Validation: negative values -> NA
  chl[chl < 0] <- NA_real_
  
  # Set unit of measurement and rounding
  data$Mishra.CHL <- units::set_units(round(chl, 1), "mg/m3")
  return(data)
}

#' @noRd
#' @keywords internal
### wisp_calc_dom_wave
wisp_calc_dom_wave <- function(data) {
  
  # CIE 1931 matrix (380-780 nm)
  cie_matrix <- matrix(c(
    380,0.0014,0.0000,0.0065, 385,0.0022,0.0001,0.0105, 390,0.0042,0.0001,0.0201,
    395,0.0076,0.0002,0.0362, 400,0.0143,0.0004,0.0679, 405,0.0232,0.0006,0.1102,
    410,0.0435,0.0012,0.2074, 415,0.0776,0.0022,0.3713, 420,0.1344,0.0040,0.6456,
    425,0.2148,0.0073,1.0391, 430,0.2839,0.0116,1.3856, 435,0.3285,0.0168,1.6230,
    440,0.3483,0.0230,1.7471, 445,0.3481,0.0298,1.7826, 450,0.3362,0.0380,1.7721,
    455,0.3187,0.0480,1.7441, 460,0.2908,0.0600,1.6692, 465,0.2511,0.0739,1.5281,
    470,0.1954,0.0910,1.2876, 475,0.1421,0.1126,1.0419, 480,0.0956,0.1390,0.8130,
    485,0.0580,0.1693,0.6162, 490,0.0320,0.2080,0.4652, 495,0.0147,0.2586,0.3533,
    500,0.0049,0.3230,0.2720, 505,0.0024,0.4073,0.2123, 510,0.0093,0.5030,0.1582,
    515,0.0291,0.6082,0.1117, 520,0.0633,0.7100,0.0782, 525,0.1096,0.7932,0.0573,
    530,0.1655,0.8620,0.0422, 535,0.2257,0.9149,0.0298, 540,0.2904,0.9540,0.0203,
    545,0.3597,0.9803,0.0134, 550,0.4334,0.9950,0.0087, 555,0.5121,1.0000,0.0057,
    560,0.5945,0.9950,0.0039, 565,0.6784,0.9786,0.0027, 570,0.7621,0.9520,0.0021,
    575,0.8425,0.9154,0.0018, 580,0.9163,0.8700,0.0017, 585,0.9786,0.8163,0.0014,
    590,1.0263,0.7570,0.0011, 595,1.0567,0.6949,0.0010, 600,1.0622,0.6310,0.0008,
    605,1.0456,0.5668,0.0006, 610,1.0026,0.5030,0.0003, 615,0.9384,0.4412,0.0002,
    620,0.8544,0.3810,0.0002, 625,0.7514,0.3210,0.0001, 630,0.6424,0.2650,0.0000,
    635,0.5419,0.2170,0.0000, 640,0.4479,0.1750,0.0000, 645,0.3608,0.1382,0.0000,
    650,0.2835,0.1070,0.0000, 655,0.2187,0.0816,0.0000, 660,0.1649,0.0610,0.0000,
    665,0.1212,0.0446,0.0000, 670,0.0874,0.0320,0.0000, 675,0.0636,0.0232,0.0000,
    680,0.0468,0.0170,0.0000, 685,0.0329,0.0119,0.0000, 690,0.0227,0.0082,0.0000,
    695,0.0158,0.0057,0.0000, 700,0.0114,0.0041,0.0000, 705,0.0081,0.0029,0.0000,
    710,0.0058,0.0021,0.0000, 715,0.0041,0.0015,0.0000, 720,0.0029,0.0010,0.0000,
    725,0.0020,0.0007,0.0000, 730,0.0014,0.0005,0.0000, 735,0.0010,0.0004,0.0000,
    740,0.0007,0.0002,0.0000, 745,0.0005,0.0002,0.0000, 750,0.0003,0.0001,0.0000,
    755,0.0002,0.0001,0.0000, 760,0.0002,0.0001,0.0000, 765,0.0001,0.0000,0.0000,
    770,0.0001,0.0000,0.0000, 775,0.0001,0.0000,0.0000, 780,0.0000,0.0000,0.0000
  ), ncol = 4, byrow = TRUE)
  
  # 1 nm interpolation
  wv_fine <- 380:780
  cie_x_f <- approx(cie_matrix[,1], cie_matrix[,2], xout = wv_fine)$y
  cie_y_f <- approx(cie_matrix[,1], cie_matrix[,3], xout = wv_fine)$y
  cie_z_f <- approx(cie_matrix[,1], cie_matrix[,4], xout = wv_fine)$y
  
  # White Point
  x_w <- 1/3
  y_w <- 1/3
  
  # Calculation of chromatic coordinates 
  sum_locus <- cie_x_f + cie_y_f + cie_z_f
  locus_x <- cie_x_f / sum_locus
  locus_y <- cie_y_f / sum_locus
  
  # Locus angles relative to the white point
  locus_angles <- (atan2(locus_y - y_w, locus_x - x_w) * 180 / pi) %% 360
  
  # Single row function
  calc_single_row <- function(row_reflectance, wv_input) {
    row_reflectance <- as.numeric(row_reflectance)
    
    # Interpolate observed reflectance on the CIE 1 nm grid
    r_interp <- approx(x = wv_input, y = row_reflectance, xout = wv_fine)$y
    r_interp[is.na(r_interp)] <- 0
    r_interp[r_interp < 0] <- 0
    
    # Calculation of tristimulus values (X, Y, Z)
    X <- sum(r_interp * cie_x_f)
    Y <- sum(r_interp * cie_y_f)
    Z <- sum(r_interp * cie_z_f)
    
    sum_XYZ <- X + Y + Z
    if (is.na(sum_XYZ) || sum_XYZ == 0) return(c(NA, NA))
    
    # Chromatic coordinates of the sample
    x_s <- X / sum_XYZ
    y_s <- Y / sum_XYZ
    
    # Sample angle
    alpha <- (atan2(y_s - y_w, x_s - x_w) * 180 / pi) %% 360
    
    # Find the wavelength on the locus with the closest angle
    angle_diff <- abs(locus_angles - alpha)
    angle_diff <- pmin(angle_diff, 360 - angle_diff)
    
    idx_dom <- which.min(angle_diff)
    lambda_dom <- wv_fine[idx_dom]
    
    return(c(alpha, lambda_dom))
  }
  
  spec_cols <- grep("^nm_", colnames(data), value = TRUE)
  wv_available <- as.numeric(gsub("nm_", "", spec_cols))
  
  res <- t(apply(data[, spec_cols], 1, calc_single_row, wv_input = wv_available))
  
  data$hue_angle <- round(res[, 1], 1)
  data$dominant_wavelength <- round(res[, 2], 0)
  
  return(data)
}

#' @noRd
#' @keywords internal
### wisp_calc_OWT_class
wisp_calc_OWT_class <- function(data) {
  
  # Standard OWT spectral signatures (Mean)
  owt_means_string <- "wavelen;1;2;3a;3b;4a;4b;5a;5b;6;7
400;0.017993;0.006752;0.003844;0.011794;0.00336;0.009152;0.002714;0.00262;0.008908;0.000067
402;0.017635;0.006785;0.003876;0.011847;0.003404;0.009229;0.0027;0.002605;0.00907;0.000068
404;0.01728;0.006816;0.003908;0.011887;0.003448;0.009297;0.002682;0.002586;0.009234;0.00007
406;0.016982;0.006852;0.00394;0.011923;0.003491;0.00936;0.002663;0.002564;0.0094;0.000072
408;0.016699;0.006887;0.003972;0.011955;0.003536;0.009421;0.002642;0.002541;0.009568;0.000074
410;0.01643;0.006924;0.004006;0.011987;0.003581;0.009481;0.002621;0.002518;0.009737;0.000076
412;0.016192;0.006965;0.004043;0.012032;0.003628;0.009549;0.002603;0.002499;0.00991;0.000078
414;0.015943;0.007004;0.004082;0.012083;0.003677;0.009623;0.002589;0.002484;0.010086;0.000081
416;0.015662;0.007041;0.004122;0.012144;0.003729;0.009706;0.002578;0.002473;0.010264;0.000083
418;0.015366;0.007075;0.004165;0.012212;0.003782;0.009795;0.002571;0.002467;0.010446;0.000085
420;0.015066;0.007109;0.00421;0.012288;0.003838;0.009891;0.002567;0.002464;0.01063;0.000088
422;0.014754;0.007141;0.004255;0.012365;0.003894;0.009989;0.002563;0.002462;0.010818;0.000091
424;0.014439;0.007171;0.004301;0.012441;0.003952;0.010088;0.002559;0.00246;0.011007;0.000094
426;0.014091;0.007194;0.004347;0.012509;0.00401;0.010183;0.002554;0.002456;0.011198;0.000097
428;0.013727;0.007211;0.004392;0.012573;0.004068;0.010276;0.002547;0.00245;0.011392;0.0001
430;0.013324;0.007219;0.004437;0.012632;0.004127;0.01037;0.00254;0.002444;0.011588;0.000103
432;0.012876;0.007214;0.00448;0.012687;0.004187;0.010466;0.002535;0.002439;0.011786;0.000106
434;0.012384;0.007193;0.004524;0.012745;0.004249;0.010567;0.002532;0.002439;0.011987;0.00011
436;0.011837;0.007153;0.004568;0.012815;0.004314;0.010685;0.002537;0.002448;0.012191;0.000113
438;0.011262;0.007096;0.004613;0.012906;0.004383;0.010827;0.002552;0.002469;0.0124;0.000117
440;0.010617;0.007006;0.004657;0.013012;0.004456;0.010993;0.002579;0.002505;0.012612;0.000121
442;0.009982;0.006901;0.004702;0.013149;0.004535;0.011193;0.00262;0.002558;0.01283;0.000125
444;0.00945;0.006809;0.004754;0.013325;0.004618;0.011424;0.002674;0.002628;0.013053;0.000129
446;0.00894;0.006706;0.004805;0.013517;0.004706;0.011677;0.002736;0.002709;0.01328;0.000134
448;0.008498;0.006611;0.004859;0.013726;0.004796;0.011947;0.002806;0.002801;0.013509;0.000138
450;0.008146;0.006537;0.004917;0.013947;0.004888;0.01222;0.002876;0.002895;0.013741;0.000143
452;0.007854;0.006477;0.004975;0.014157;0.004979;0.012482;0.002941;0.002984;0.013974;0.000148
454;0.007645;0.006447;0.005037;0.014371;0.00507;0.012735;0.003001;0.003067;0.014208;0.000152
456;0.007481;0.006432;0.005101;0.014582;0.00516;0.012981;0.003056;0.003142;0.014443;0.000157
458;0.007332;0.006422;0.005164;0.014784;0.00525;0.013219;0.003106;0.003208;0.014679;0.000162
460;0.007206;0.006419;0.005227;0.014986;0.00534;0.013455;0.003154;0.003269;0.014916;0.000167
462;0.007081;0.006415;0.00529;0.015188;0.005431;0.013693;0.003201;0.003325;0.015156;0.000173
464;0.00694;0.006398;0.00535;0.015387;0.005522;0.013939;0.003252;0.003382;0.015397;0.000178
466;0.006804;0.006382;0.005411;0.015598;0.005616;0.014197;0.003306;0.00344;0.015642;0.000184
468;0.006644;0.006348;0.005469;0.015809;0.005711;0.014469;0.003366;0.003502;0.015888;0.00019
470;0.00648;0.006305;0.005527;0.016031;0.005808;0.014758;0.003431;0.003569;0.016138;0.000196
472;0.006318;0.00626;0.005584;0.016265;0.005908;0.015063;0.003503;0.003641;0.01639;0.000202
474;0.00615;0.006205;0.00564;0.016496;0.006009;0.015376;0.003578;0.003715;0.016644;0.000208
476;0.005985;0.006146;0.005693;0.016722;0.006111;0.015689;0.003654;0.00379;0.0169;0.000215
478;0.005818;0.00608;0.005743;0.016935;0.006211;0.016;0.003729;0.003863;0.017157;0.000221
480;0.005648;0.006005;0.005788;0.017127;0.00631;0.0163;0.003802;0.003933;0.017414;0.000228
482;0.005481;0.005927;0.005829;0.017298;0.006408;0.016589;0.003872;0.003999;0.017672;0.000235
484;0.005303;0.005834;0.005863;0.017439;0.006504;0.016864;0.00394;0.004062;0.017929;0.000242
486;0.005119;0.005731;0.005891;0.017555;0.006598;0.017131;0.004006;0.004124;0.018187;0.000249
488;0.004925;0.005612;0.00591;0.017647;0.006691;0.017395;0.004074;0.004191;0.018446;0.000256
490;0.004698;0.005455;0.005914;0.017693;0.006782;0.017658;0.004149;0.004266;0.018706;0.000263
492;0.00446;0.00528;0.005907;0.017716;0.006873;0.017931;0.004233;0.004354;0.018968;0.00027
494;0.004214;0.005085;0.00589;0.017719;0.006966;0.018222;0.004332;0.004462;0.019232;0.000278
496;0.003959;0.00487;0.00586;0.017701;0.00706;0.018538;0.00445;0.004594;0.019499;0.000286
498;0.003704;0.00464;0.005818;0.01766;0.007156;0.018879;0.004588;0.004753;0.019769;0.000294
500;0.003451;0.004399;0.005762;0.017588;0.007251;0.019237;0.004745;0.00494;0.02004;0.000302
502;0.003195;0.004143;0.00569;0.017475;0.007346;0.019616;0.004925;0.005159;0.020314;0.00031
504;0.002924;0.003856;0.005587;0.01727;0.007436;0.020002;0.00513;0.005415;0.020589;0.000319
506;0.002675;0.00358;0.005473;0.017024;0.007523;0.020385;0.005353;0.005704;0.020865;0.000328
508;0.002431;0.003299;0.005336;0.016693;0.0076;0.020747;0.005594;0.006026;0.021139;0.000337
510;0.002221;0.003049;0.005201;0.016361;0.007677;0.021105;0.005853;0.006382;0.021413;0.000346
512;0.002061;0.002856;0.005093;0.016099;0.007759;0.021465;0.006119;0.006755;0.021688;0.000356
514;0.001924;0.002689;0.004995;0.015861;0.007841;0.021819;0.006394;0.00715;0.021963;0.000365
516;0.001829;0.002573;0.004935;0.01574;0.007937;0.022204;0.006679;0.007564;0.022242;0.000375
518;0.001772;0.002506;0.004919;0.015762;0.00805;0.02263;0.006973;0.00799;0.022525;0.000386
520;0.001732;0.002461;0.004922;0.015841;0.00817;0.023061;0.007267;0.008415;0.022809;0.000397
522;0.001706;0.002435;0.00494;0.015959;0.008294;0.023483;0.007553;0.00882;0.023092;0.000408
524;0.001683;0.002414;0.004962;0.016082;0.008417;0.023888;0.007831;0.009209;0.023375;0.000419
526;0.001656;0.002386;0.004973;0.016155;0.008532;0.024247;0.008092;0.00957;0.023655;0.000431
528;0.001631;0.002361;0.004985;0.016218;0.008643;0.024575;0.008334;0.009898;0.023932;0.000443
530;0.001602;0.00233;0.004986;0.016235;0.008744;0.024858;0.008555;0.010188;0.024206;0.000455
532;0.00157;0.002294;0.00498;0.016219;0.008839;0.025113;0.008762;0.010448;0.024478;0.000467
534;0.001535;0.002253;0.004964;0.016163;0.008925;0.025333;0.008955;0.010677;0.024747;0.00048
536;0.001497;0.002209;0.004941;0.016078;0.009005;0.025522;0.009132;0.010875;0.025013;0.000493
538;0.001456;0.002159;0.004907;0.015955;0.009076;0.025683;0.0093;0.011049;0.025276;0.000506
540;0.001411;0.002101;0.004861;0.015787;0.009138;0.025812;0.009458;0.011204;0.025536;0.000519
542;0.001364;0.002042;0.004809;0.015596;0.009192;0.025918;0.009608;0.011343;0.025793;0.000532
544;0.001313;0.001974;0.00474;0.015346;0.009233;0.025983;0.009746;0.011467;0.026046;0.000546
546;0.001262;0.001907;0.004669;0.015091;0.009271;0.026036;0.009881;0.01158;0.026297;0.00056
548;0.001212;0.00184;0.004593;0.014816;0.009302;0.026068;0.010011;0.011681;0.026544;0.000574
550;0.001164;0.001775;0.004516;0.014542;0.00933;0.026089;0.010136;0.011769;0.02679;0.000588
552;0.001125;0.001721;0.004455;0.014323;0.009367;0.026138;0.010264;0.011853;0.027037;0.000603
554;0.00109;0.001674;0.004402;0.014127;0.009406;0.026187;0.010384;0.01192;0.027284;0.000618
556;0.001063;0.00164;0.004366;0.013988;0.009455;0.026258;0.010498;0.011968;0.027532;0.000634
558;0.001041;0.001611;0.00434;0.013879;0.009509;0.026334;0.010604;0.011993;0.02778;0.00065
560;0.001016;0.001579;0.004304;0.013735;0.009552;0.026373;0.010684;0.01199;0.028023;0.000667
562;0.000994;0.001549;0.004273;0.013603;0.009594;0.026404;0.010738;0.011954;0.028264;0.000683
564;0.000969;0.001516;0.004234;0.013443;0.009627;0.026412;0.010775;0.011902;0.028502;0.0007
566;0.000941;0.001478;0.004185;0.013254;0.009652;0.026405;0.010802;0.011841;0.028736;0.000717
568;0.00091;0.001434;0.004123;0.013026;0.009664;0.026378;0.010813;0.011773;0.028966;0.000735
570;0.000875;0.001383;0.004048;0.012759;0.009661;0.026337;0.010806;0.0117;0.029191;0.000752
572;0.000839;0.001329;0.003966;0.012482;0.009652;0.02632;0.010795;0.011637;0.029416;0.00077
574;0.000794;0.001262;0.003855;0.012119;0.009614;0.026257;0.010775;0.011585;0.029631;0.000788
576;0.000747;0.001189;0.003726;0.011699;0.009554;0.026155;0.01075;0.011547;0.029839;0.000805
578;0.000696;0.00111;0.003578;0.011219;0.009469;0.025998;0.010724;0.011529;0.030035;0.000822
580;0.000643;0.001028;0.00341;0.010674;0.009352;0.025756;0.010684;0.011517;0.030216;0.000838
582;0.00059;0.000944;0.003226;0.01007;0.009199;0.025385;0.010601;0.011478;0.030376;0.000853
584;0.000538;0.000863;0.003034;0.009438;0.009018;0.024914;0.010495;0.011427;0.030518;0.000867
586;0.00049;0.000788;0.002842;0.008808;0.008818;0.024368;0.010374;0.011372;0.030642;0.00088
588;0.000444;0.000715;0.002645;0.00816;0.008589;0.023722;0.010236;0.011314;0.030743;0.000892
590;0.000398;0.000642;0.002435;0.007475;0.008317;0.022931;0.010061;0.011242;0.03081;0.000901
592;0.000358;0.000577;0.002237;0.00683;0.008035;0.022095;0.009872;0.011159;0.030856;0.000909
594;0.000319;0.000515;0.002038;0.006187;0.007724;0.021169;0.009662;0.011077;0.030869;0.000914
596;0.000286;0.000463;0.001864;0.00563;0.007429;0.020287;0.009457;0.010995;0.030873;0.000919
598;0.000258;0.000419;0.001713;0.005146;0.007152;0.019449;0.009248;0.010903;0.030868;0.000924
600;0.000236;0.000383;0.001587;0.004748;0.006909;0.018704;0.009048;0.010796;0.030868;0.000929
602;0.00022;0.000358;0.001496;0.00446;0.006727;0.018137;0.008882;0.010682;0.0309;0.000937
604;0.000208;0.000338;0.001427;0.00424;0.006586;0.017685;0.008732;0.010546;0.030954;0.000946
606;0.0002;0.000327;0.001388;0.004115;0.006513;0.017424;0.008617;0.010393;0.031056;0.00096
608;0.000195;0.00032;0.001364;0.004035;0.006473;0.017252;0.008515;0.01022;0.03118;0.000975
610;0.000193;0.000316;0.001354;0.003998;0.006468;0.017176;0.008436;0.010041;0.031332;0.000993
612;0.000191;0.000314;0.00135;0.003982;0.006479;0.017139;0.008365;0.009858;0.031494;0.001012
614;0.000189;0.000311;0.001343;0.003954;0.006479;0.017073;0.008284;0.009676;0.031644;0.00103
616;0.000187;0.000309;0.001338;0.003935;0.006487;0.017028;0.00821;0.009507;0.031799;0.00105
618;0.000185;0.000306;0.001331;0.003907;0.006486;0.016963;0.008131;0.009351;0.031944;0.001068
620;0.000182;0.000303;0.001319;0.003867;0.006474;0.016867;0.008044;0.009205;0.032077;0.001086
622;0.000179;0.000299;0.001308;0.003827;0.00646;0.016767;0.007955;0.00907;0.032205;0.001104
624;0.000177;0.000295;0.001295;0.003786;0.006445;0.016665;0.007866;0.008947;0.032329;0.001122
626;0.000174;0.000291;0.001283;0.003744;0.006428;0.016563;0.007779;0.00884;0.03245;0.00114
628;0.000171;0.000287;0.001271;0.003707;0.006415;0.016475;0.007707;0.008761;0.032573;0.001159
630;0.000169;0.000284;0.001259;0.003668;0.0064;0.016391;0.007651;0.008713;0.032693;0.001177
632;0.000166;0.000279;0.001246;0.003625;0.006381;0.016299;0.007602;0.008684;0.032808;0.001196
634;0.000163;0.000275;0.001233;0.003583;0.006363;0.016214;0.007565;0.008672;0.032921;0.001215
636;0.000161;0.000271;0.001219;0.003541;0.006343;0.01613;0.007536;0.008669;0.033031;0.001233
638;0.000158;0.000267;0.001205;0.003497;0.00632;0.016045;0.007514;0.008673;0.033137;0.001252
640;0.000155;0.000262;0.001189;0.003449;0.006293;0.015952;0.007496;0.008684;0.033235;0.00127
642;0.000152;0.000258;0.001172;0.003398;0.006262;0.015854;0.007482;0.008701;0.033328;0.001288
644;0.000148;0.000253;0.001154;0.003345;0.006227;0.015745;0.007464;0.008719;0.033415;0.001306
646;0.000145;0.000247;0.001133;0.003281;0.006181;0.015604;0.007438;0.008738;0.033487;0.001322
648;0.000141;0.000241;0.001109;0.003207;0.006122;0.015419;0.007386;0.008739;0.03354;0.001336
650;0.000137;0.000234;0.001081;0.00312;0.006047;0.015177;0.007292;0.008703;0.033568;0.001347
652;0.000131;0.000225;0.001044;0.003008;0.00594;0.014839;0.007142;0.008609;0.033552;0.001353
654;0.000126;0.000216;0.001006;0.00289;0.00582;0.014447;0.006935;0.008435;0.033509;0.001356
656;0.00012;0.000206;0.000964;0.002758;0.005678;0.013979;0.006654;0.008158;0.033424;0.001354
658;0.000114;0.000197;0.000923;0.002631;0.005534;0.013491;0.006322;0.007786;0.03332;0.001348
660;0.00011;0.000189;0.000889;0.002523;0.005406;0.013028;0.005956;0.007337;0.033219;0.001343
662;0.000106;0.000183;0.00086;0.00243;0.005292;0.012596;0.005587;0.006857;0.033124;0.001338
664;0.000103;0.000178;0.00084;0.002362;0.005209;0.012239;0.005247;0.006394;0.033061;0.001336
666;0.0001;0.000175;0.000824;0.002306;0.00514;0.01192;0.004937;0.005971;0.033013;0.001336
668;0.000099;0.000172;0.000813;0.002264;0.005089;0.011659;0.004676;0.005612;0.032989;0.001339
670;0.000097;0.00017;0.000803;0.00223;0.005048;0.011446;0.00447;0.005331;0.032984;0.001344
672;0.000096;0.000168;0.000794;0.002199;0.005012;0.011273;0.004319;0.005132;0.03299;0.001351
674;0.000095;0.000166;0.000787;0.002177;0.004988;0.01116;0.004224;0.005008;0.03302;0.001362
676;0.000093;0.000164;0.000779;0.002156;0.004965;0.011086;0.004184;0.004961;0.033061;0.001374
678;0.000092;0.000162;0.00077;0.002135;0.004942;0.011053;0.004203;0.005;0.033113;0.001388
680;0.00009;0.000159;0.00076;0.002115;0.004921;0.011063;0.004288;0.00514;0.033177;0.001404
682;0.000088;0.000156;0.000749;0.002096;0.004903;0.011124;0.004452;0.00541;0.03326;0.001424
684;0.000087;0.000153;0.000738;0.002076;0.004886;0.011222;0.0047;0.005833;0.033356;0.001447
686;0.000085;0.000149;0.000725;0.002054;0.004866;0.011346;0.005035;0.006433;0.033458;0.001472
688;0.000082;0.000145;0.000711;0.002028;0.004843;0.011467;0.00544;0.007233;0.033559;0.0015
690;0.00008;0.000141;0.000694;0.001988;0.004804;0.011529;0.005872;0.008232;0.033631;0.001525
692;0.000077;0.000136;0.000677;0.001944;0.004756;0.011548;0.006302;0.009386;0.033684;0.001549
694;0.000075;0.000132;0.000657;0.001888;0.004691;0.011492;0.006704;0.010699;0.033697;0.001568
696;0.000072;0.000126;0.000634;0.001824;0.004608;0.011362;0.007056;0.012157;0.03367;0.001583
698;0.000068;0.00012;0.000607;0.001745;0.004496;0.011123;0.0073;0.013631;0.033574;0.001587
700;0.000064;0.000113;0.000575;0.001649;0.004349;0.010769;0.007419;0.015094;0.033396;0.00158
702;0.00006;0.000107;0.000542;0.001553;0.004192;0.010368;0.007443;0.016484;0.033177;0.001566
704;0.000056;0.0001;0.000508;0.001451;0.004017;0.009902;0.007357;0.01773;0.032893;0.001543
706;0.000052;0.000092;0.000473;0.001346;0.003825;0.009388;0.007174;0.018804;0.032546;0.00151
708;0.000048;0.000085;0.000437;0.001242;0.003625;0.008845;0.006915;0.019692;0.032144;0.00147
710;0.000044;0.000078;0.000403;0.001141;0.003419;0.008286;0.006595;0.020373;0.03169;0.001424
712;0.00004;0.000072;0.000369;0.001043;0.003212;0.007724;0.006236;0.020848;0.031189;0.001372
714;0.000037;0.000065;0.000338;0.000952;0.003011;0.007181;0.005861;0.021134;0.030658;0.001317
716;0.000034;0.00006;0.000309;0.000869;0.002815;0.006659;0.00548;0.02124;0.030099;0.00126
718;0.000031;0.000055;0.000283;0.000792;0.00263;0.006166;0.005106;0.021181;0.029521;0.001202
720;0.000028;0.00005;0.000258;0.000721;0.002451;0.005696;0.004738;0.020971;0.028916;0.001144
722;0.000025;0.000045;0.000234;0.000652;0.002268;0.005222;0.004359;0.020583;0.028242;0.00108
724;0.000023;0.000041;0.000213;0.000591;0.002099;0.00479;0.004006;0.020106;0.027564;0.001018
726;0.000021;0.000037;0.000192;0.000533;0.001934;0.004372;0.003661;0.019495;0.02684;0.000955
728;0.000019;0.000034;0.000175;0.000484;0.001787;0.004005;0.003355;0.018852;0.02614;0.000896
730;0.000017;0.000031;0.000161;0.000443;0.001662;0.0037;0.003099;0.018247;0.025501;0.000845
732;0.000016;0.000028;0.000148;0.000409;0.001552;0.003433;0.002873;0.017653;0.024897;0.000799
734;0.000015;0.000027;0.000139;0.000383;0.001469;0.003235;0.002706;0.017192;0.024422;0.000764
736;0.000014;0.000025;0.000133;0.000366;0.001413;0.003102;0.002595;0.01689;0.024095;0.000741
738;0.000014;0.000025;0.000129;0.000354;0.001373;0.003007;0.002516;0.01668;0.02386;0.000726
740;0.000013;0.000024;0.000126;0.000347;0.001349;0.00295;0.002469;0.016577;0.023726;0.000718
742;0.000013;0.000024;0.000124;0.000342;0.001333;0.002915;0.00244;0.016531;0.02365;0.000714
744;0.000013;0.000023;0.000123;0.000339;0.001322;0.002889;0.00242;0.016509;0.023601;0.000711
746;0.000013;0.000023;0.000123;0.000337;0.001318;0.002878;0.002411;0.016527;0.023594;0.000712
748;0.000013;0.000023;0.000122;0.000337;0.001315;0.002872;0.002408;0.01656;0.023604;0.000714
750;0.000013;0.000023;0.000122;0.000336;0.001316;0.002873;0.00241;0.016608;0.023631;0.000717
752;0.000013;0.000023;0.000122;0.000337;0.001318;0.002879;0.002417;0.016671;0.023674;0.000722
754;0.000013;0.000023;0.000123;0.000338;0.001322;0.002889;0.002426;0.016739;0.023725;0.000727
756;0.000013;0.000023;0.000123;0.00034;0.001328;0.002903;0.002439;0.016822;0.02379;0.000733
758;0.000013;0.000023;0.000124;0.000341;0.001335;0.002918;0.002453;0.016905;0.023857;0.000739
760;0.000013;0.000023;0.000125;0.000343;0.001342;0.002936;0.00247;0.016997;0.023933;0.000746
762;0.000013;0.000024;0.000126;0.000347;0.001354;0.002962;0.002494;0.017114;0.024031;0.000755
764;0.000013;0.000024;0.000127;0.000351;0.001368;0.002996;0.002524;0.017255;0.024148;0.000766
766;0.000013;0.000024;0.000129;0.000355;0.001384;0.003034;0.002558;0.017412;0.024278;0.000777
768;0.000013;0.000024;0.000131;0.000361;0.001403;0.003079;0.002598;0.01759;0.024426;0.000791
770;0.000014;0.000025;0.000133;0.000367;0.001425;0.003131;0.002644;0.017788;0.024589;0.000805
772;0.000014;0.000025;0.000136;0.000374;0.00145;0.00319;0.002696;0.018009;0.024769;0.000822
774;0.000014;0.000026;0.000139;0.000382;0.001477;0.003255;0.002754;0.018248;0.024962;0.00084
776;0.000014;0.000026;0.000142;0.000391;0.001506;0.003323;0.002814;0.018494;0.02516;0.000858
778;0.000015;0.000027;0.000145;0.0004;0.001536;0.003394;0.002878;0.018749;0.025364;0.000878
780;0.000015;0.000027;0.000148;0.000409;0.001569;0.003472;0.002947;0.01902;0.025578;0.000899
782;0.000015;0.000028;0.000152;0.000419;0.001602;0.003551;0.003017;0.019293;0.025792;0.000921
784;0.000015;0.000028;0.000155;0.000429;0.001636;0.003631;0.003089;0.019565;0.026003;0.000942
786;0.000016;0.000029;0.000159;0.000439;0.001668;0.00371;0.003159;0.01983;0.026207;0.000964
788;0.000016;0.00003;0.000162;0.000448;0.001698;0.003781;0.003222;0.020063;0.026386;0.000984
790;0.000016;0.00003;0.000165;0.000456;0.001725;0.003846;0.00328;0.020278;0.026551;0.001002
792;0.000016;0.00003;0.000167;0.000463;0.001747;0.0039;0.003329;0.020457;0.026688;0.001018
794;0.000017;0.000031;0.000169;0.000469;0.001765;0.003942;0.003368;0.020598;0.026798;0.001032
796;0.000017;0.000031;0.000171;0.000472;0.001777;0.003971;0.003394;0.020697;0.026877;0.001042
798;0.000017;0.000031;0.000171;0.000474;0.001783;0.003984;0.003407;0.020748;0.02692;0.001048
800;0.000017;0.000031;0.000171;0.000473;0.001781;0.003979;0.003403;0.02074;0.026922;0.00105
802;0.000017;0.000031;0.00017;0.00047;0.00177;0.003952;0.00338;0.020666;0.026876;0.001047
804;0.000016;0.00003;0.000167;0.000464;0.001751;0.003907;0.003341;0.020536;0.026788;0.001039
806;0.000016;0.00003;0.000164;0.000455;0.001724;0.003839;0.003283;0.020335;0.026649;0.001026
808;0.000016;0.000029;0.000161;0.000445;0.00169;0.003758;0.003211;0.020087;0.026473;0.001009
810;0.000015;0.000028;0.000156;0.000432;0.001649;0.003659;0.003125;0.019778;0.026251;0.000987
812;0.000015;0.000027;0.000151;0.000418;0.001601;0.003543;0.003023;0.019407;0.025979;0.000961
814;0.000014;0.000026;0.000145;0.000402;0.001548;0.003415;0.002912;0.018992;0.025669;0.000932
816;0.000013;0.000025;0.000139;0.000383;0.001487;0.00327;0.002785;0.018504;0.025298;0.000897
818;0.000013;0.000024;0.000132;0.000364;0.001422;0.003116;0.00265;0.01797;0.024884;0.00086
820;0.000012;0.000022;0.000125;0.000346;0.001359;0.002968;0.002522;0.017445;0.024468;0.000824
822;0.000011;0.000021;0.000119;0.000327;0.001293;0.002814;0.002388;0.016881;0.024011;0.000786
824;0.000011;0.00002;0.000112;0.000309;0.001229;0.002665;0.002258;0.016316;0.023543;0.000748
826;0.00001;0.000019;0.000106;0.000293;0.001169;0.002528;0.00214;0.015782;0.023091;0.000713
828;0.00001;0.000018;0.000101;0.000277;0.001114;0.0024;0.00203;0.01527;0.022647;0.00068
830;0.000009;0.000017;0.000096;0.000265;0.001067;0.002293;0.001938;0.014832;0.02226;0.000653
832;0.000009;0.000016;0.000092;0.000254;0.001027;0.002202;0.00186;0.014449;0.021916;0.000629
834;0.000008;0.000016;0.000089;0.000245;0.000993;0.002126;0.001794;0.014122;0.021617;0.00061
836;0.000008;0.000015;0.000086;0.000237;0.000963;0.002059;0.001737;0.013832;0.021349;0.000592
838;0.000008;0.000015;0.000083;0.000229;0.000934;0.001994;0.001681;0.013542;0.021078;0.000576
840;0.000008;0.000014;0.000081;0.000222;0.000906;0.001931;0.001628;0.013261;0.020811;0.000559
842;0.000007;0.000014;0.000078;0.000214;0.000877;0.001867;0.001573;0.012967;0.020528;0.000542
844;0.000007;0.000013;0.000075;0.000207;0.00085;0.001806;0.001522;0.012685;0.020253;0.000526
846;0.000007;0.000013;0.000073;0.000201;0.000825;0.001752;0.001476;0.01243;0.020001;0.000512
848;0.000007;0.000012;0.000071;0.000195;0.000802;0.001701;0.001433;0.012186;0.019756;0.000498
850;0.000006;0.000012;0.000069;0.00019;0.000782;0.001657;0.001395;0.011971;0.019537;0.000487
852;0.000006;0.000012;0.000067;0.000185;0.000763;0.001615;0.00136;0.011767;0.019328;0.000476
854;0.000006;0.000012;0.000066;0.00018;0.000745;0.001576;0.001328;0.011575;0.019129;0.000465
856;0.000006;0.000011;0.000064;0.000176;0.000729;0.001541;0.001298;0.0114;0.018945;0.000456
858;0.000006;0.000011;0.000063;0.000172;0.000714;0.001509;0.001271;0.011236;0.018772;0.000447
860;0.000006;0.000011;0.000062;0.000169;0.000701;0.001481;0.001247;0.011092;0.018618;0.00044
862;0.000006;0.000011;0.000061;0.000166;0.00069;0.001455;0.001226;0.010961;0.018477;0.000433
864;0.000005;0.00001;0.00006;0.000163;0.000678;0.00143;0.001205;0.010834;0.018339;0.000426
866;0.000005;0.00001;0.000059;0.000161;0.000668;0.001407;0.001186;0.010714;0.018208;0.00042
868;0.000005;0.00001;0.000058;0.000158;0.000657;0.001385;0.001167;0.010597;0.01808;0.000414
870;0.000005;0.00001;0.000057;0.000156;0.000647;0.001362;0.001148;0.010478;0.017948;0.000408
872;0.000005;0.00001;0.000056;0.000153;0.000637;0.00134;0.00113;0.010361;0.017817;0.000402
874;0.000005;0.00001;0.000055;0.000151;0.000627;0.001318;0.001112;0.010244;0.017686;0.000396
876;0.000005;0.000009;0.000054;0.000148;0.000617;0.001297;0.001094;0.010128;0.017555;0.000391
878;0.000005;0.000009;0.000053;0.000146;0.000607;0.001276;0.001076;0.010015;0.017426;0.000385
880;0.000005;0.000009;0.000052;0.000143;0.000598;0.001256;0.00106;0.009907;0.017302;0.00038
882;0.000005;0.000009;0.000052;0.000141;0.000589;0.001237;0.001044;0.0098;0.017178;0.000374
884;0.000005;0.000009;0.000051;0.000139;0.00058;0.001218;0.001028;0.009695;0.017056;0.000369
886;0.000005;0.000009;0.00005;0.000137;0.000572;0.001199;0.001012;0.009589;0.016932;0.000364
888;0.000004;0.000008;0.000049;0.000135;0.000563;0.00118;0.000996;0.00948;0.016803;0.000359
890;0.000004;0.000008;0.000048;0.000133;0.000553;0.00116;0.000979;0.009367;0.01667;0.000353
892;0.000004;0.000008;0.000048;0.00013;0.000544;0.001139;0.000962;0.009249;0.016529;0.000347
894;0.000004;0.000008;0.000047;0.000128;0.000534;0.001119;0.000945;0.009128;0.016383;0.000341
896;0.000004;0.000008;0.000046;0.000126;0.000526;0.0011;0.000929;0.00902;0.016253;0.000336
898;0.000004;0.000008;0.000045;0.000123;0.000516;0.00108;0.000912;0.008902;0.016109;0.000331
900;0.000004;0.000008;0.000044;0.000121;0.000508;0.001062;0.000897;0.008796;0.015979;0.000325
"
  
  # Standard OWT spectral signatures (Standard Deviation)
  owt_sds_string <- "wavelen;1;2;3a;3b;4a;4b;5a;5b;6;7
400;0.012619;0.003746;0.004319;0.008004;0.004588;0.007984;0.002253;0.001997;0.005921;0.000088
402;0.012212;0.003765;0.004376;0.00803;0.004659;0.008013;0.002258;0.001995;0.006;0.000091
404;0.011821;0.003782;0.004435;0.008051;0.004731;0.008034;0.002261;0.001991;0.006081;0.000094
406;0.01151;0.003803;0.004494;0.008071;0.004804;0.008053;0.002263;0.001985;0.006162;0.000096
408;0.011222;0.003825;0.004555;0.008091;0.004878;0.00807;0.002265;0.001978;0.006244;0.000099
410;0.010957;0.003847;0.004618;0.008114;0.004953;0.008088;0.002267;0.001972;0.006327;0.000102
412;0.010727;0.003872;0.004682;0.008147;0.00503;0.008113;0.00227;0.001968;0.006411;0.000105
414;0.01048;0.003895;0.004747;0.008184;0.005108;0.008145;0.002275;0.001965;0.006496;0.000108
416;0.010188;0.003916;0.004813;0.008228;0.005187;0.008183;0.002281;0.001965;0.006581;0.000112
418;0.009877;0.003934;0.00488;0.008277;0.005268;0.008228;0.00229;0.001967;0.006668;0.000115
420;0.009559;0.003951;0.004948;0.00833;0.005351;0.008277;0.002299;0.001971;0.006755;0.000119
422;0.009228;0.003966;0.005017;0.008384;0.005434;0.008328;0.002309;0.001975;0.006844;0.000123
424;0.008897;0.003979;0.005086;0.008439;0.005518;0.00838;0.002318;0.001979;0.006933;0.000127
426;0.008537;0.003988;0.005155;0.008491;0.005603;0.008429;0.002327;0.001982;0.007023;0.000131
428;0.008165;0.003993;0.005223;0.008542;0.005689;0.008477;0.002335;0.001983;0.007113;0.000136
430;0.007754;0.003991;0.005291;0.00859;0.005776;0.008524;0.002343;0.001985;0.007205;0.00014
432;0.007303;0.00398;0.005357;0.008637;0.005864;0.008573;0.002351;0.001987;0.007297;0.000145
434;0.006819;0.003961;0.005421;0.008684;0.005952;0.008626;0.002361;0.001991;0.007389;0.00015
436;0.006288;0.003929;0.005483;0.008735;0.006042;0.008691;0.002375;0.002001;0.007483;0.000155
438;0.005739;0.003886;0.005543;0.008792;0.006132;0.008771;0.002395;0.002018;0.007578;0.00016
440;0.005142;0.003824;0.005596;0.008852;0.006224;0.008869;0.002421;0.002043;0.007674;0.000166
442;0.004575;0.003755;0.005647;0.008922;0.006316;0.008989;0.002455;0.002078;0.00777;0.000172
444;0.004115;0.003696;0.005699;0.009007;0.00641;0.009129;0.002495;0.002121;0.007868;0.000178
446;0.003692;0.003634;0.005749;0.009098;0.006506;0.009283;0.002541;0.002172;0.007966;0.000184
448;0.003339;0.003579;0.0058;0.009195;0.006602;0.009448;0.00259;0.002228;0.008065;0.00019
450;0.003066;0.003538;0.005856;0.0093;0.0067;0.009613;0.00264;0.002285;0.008165;0.000196
452;0.002852;0.003509;0.005914;0.0094;0.006798;0.009769;0.002687;0.00234;0.008264;0.000203
454;0.002701;0.003497;0.005978;0.009503;0.006898;0.009916;0.002732;0.002391;0.008365;0.00021
456;0.002586;0.003495;0.006047;0.009606;0.006999;0.010057;0.002773;0.002438;0.008466;0.000217
458;0.002487;0.003497;0.006116;0.009706;0.007101;0.010191;0.002812;0.002479;0.008567;0.000224
460;0.002405;0.003504;0.006188;0.009805;0.007203;0.010324;0.002849;0.002518;0.008668;0.000231
462;0.002327;0.00351;0.006259;0.009904;0.007306;0.010457;0.002885;0.002553;0.00877;0.000239
464;0.002242;0.003511;0.006327;0.010002;0.007409;0.010596;0.002923;0.002588;0.008872;0.000247
466;0.002163;0.003513;0.006395;0.010103;0.007513;0.010743;0.002962;0.002624;0.008975;0.000255
468;0.002071;0.003505;0.006459;0.010205;0.007617;0.010899;0.003003;0.002661;0.009078;0.000263
470;0.001979;0.003494;0.006521;0.01031;0.007721;0.011066;0.003047;0.002701;0.009181;0.000272
472;0.001891;0.003483;0.006581;0.01042;0.007826;0.011242;0.003094;0.002743;0.009284;0.000281
474;0.001805;0.003467;0.006639;0.010528;0.007931;0.011421;0.003142;0.002787;0.009388;0.00029
476;0.001725;0.003451;0.006695;0.010634;0.008036;0.011601;0.003191;0.002831;0.009492;0.000299
478;0.001648;0.003431;0.00675;0.010735;0.008141;0.011779;0.00324;0.002874;0.009596;0.000308
480;0.001575;0.003409;0.006801;0.010828;0.008245;0.011949;0.003287;0.002916;0.0097;0.000318
482;0.001507;0.003384;0.00685;0.010912;0.008349;0.012111;0.003333;0.002955;0.009805;0.000327
484;0.00144;0.003353;0.006893;0.010983;0.008451;0.012264;0.003378;0.002993;0.009909;0.000337
486;0.001376;0.003317;0.006931;0.011044;0.008552;0.012411;0.003422;0.003031;0.010013;0.000347
488;0.001313;0.003274;0.006962;0.011095;0.008652;0.012554;0.003467;0.003071;0.010117;0.000357
490;0.00124;0.00321;0.006977;0.011128;0.008748;0.012697;0.003516;0.003115;0.01022;0.000368
492;0.001171;0.003135;0.006981;0.011152;0.008842;0.012845;0.003569;0.003166;0.010324;0.000378
494;0.001103;0.00305;0.006972;0.01117;0.008933;0.013005;0.003629;0.003228;0.010428;0.000389
496;0.001038;0.002951;0.006949;0.011182;0.009021;0.01318;0.003698;0.003301;0.010531;0.0004
498;0.000978;0.002843;0.006911;0.011189;0.009105;0.013369;0.003776;0.003387;0.010634;0.000411
500;0.000923;0.002726;0.00686;0.01119;0.009185;0.01357;0.003863;0.003488;0.010736;0.000422
502;0.00087;0.002598;0.006789;0.011181;0.009259;0.013786;0.00396;0.003603;0.010839;0.000434
504;0.000817;0.002448;0.006687;0.011143;0.009324;0.014007;0.004068;0.003735;0.01094;0.000446
506;0.000768;0.002301;0.006575;0.011096;0.009383;0.01423;0.004183;0.003882;0.011041;0.000458
508;0.00072;0.002146;0.006441;0.01102;0.009433;0.014445;0.004306;0.004044;0.011141;0.000471
510;0.000678;0.002007;0.00631;0.010944;0.009481;0.014661;0.004436;0.004221;0.011241;0.000484
512;0.000647;0.001899;0.006206;0.010892;0.009535;0.014878;0.00457;0.004405;0.011341;0.000497
514;0.000621;0.001804;0.006112;0.010846;0.009591;0.015094;0.004707;0.004598;0.011441;0.00051
516;0.000604;0.001741;0.006055;0.010845;0.00966;0.015325;0.004849;0.004798;0.011542;0.000524
518;0.000597;0.001707;0.006042;0.010899;0.009744;0.015575;0.004995;0.005002;0.011644;0.000539
520;0.000594;0.001687;0.006049;0.010973;0.009835;0.015825;0.00514;0.005204;0.011746;0.000554
522;0.000594;0.001678;0.006073;0.011057;0.009932;0.016064;0.005282;0.005393;0.011849;0.00057
524;0.000595;0.001673;0.006102;0.01114;0.01003;0.016291;0.005419;0.005573;0.011951;0.000586
526;0.000595;0.001664;0.006122;0.011197;0.010125;0.016488;0.00555;0.00574;0.012052;0.000603
528;0.000594;0.001656;0.006144;0.011246;0.010219;0.016664;0.005673;0.00589;0.012153;0.00062
530;0.000592;0.001644;0.006158;0.011272;0.010309;0.016811;0.005788;0.006023;0.012253;0.000637
532;0.000588;0.001628;0.006164;0.011282;0.010395;0.016941;0.005895;0.00614;0.012352;0.000655
534;0.000583;0.001608;0.006163;0.011272;0.010477;0.01705;0.005996;0.006242;0.01245;0.000673
536;0.000576;0.001586;0.006155;0.011248;0.010556;0.017142;0.00609;0.00633;0.012547;0.000691
538;0.000568;0.00156;0.006136;0.011207;0.010629;0.017219;0.006181;0.006408;0.012644;0.00071
540;0.000558;0.001528;0.006106;0.011145;0.010697;0.017281;0.006269;0.006479;0.012739;0.000729
542;0.000546;0.001494;0.006069;0.011072;0.01076;0.017332;0.006354;0.006544;0.012833;0.000748
544;0.000532;0.001453;0.006014;0.010971;0.010814;0.017364;0.006436;0.006604;0.012926;0.000768
546;0.000519;0.001413;0.005957;0.010866;0.010866;0.01739;0.006516;0.006662;0.013018;0.000788
548;0.000504;0.001371;0.005893;0.010749;0.010914;0.017407;0.006595;0.006715;0.01311;0.000808
550;0.00049;0.00133;0.005827;0.01063;0.010959;0.01742;0.006672;0.006765;0.0132;0.000829
552;0.000479;0.001297;0.005776;0.010535;0.011011;0.017444;0.006749;0.006813;0.013291;0.000851
554;0.000469;0.001269;0.005733;0.010449;0.011064;0.017466;0.006823;0.006857;0.013381;0.000873
556;0.000462;0.001249;0.005707;0.010387;0.011126;0.017495;0.006898;0.006895;0.013471;0.000896
558;0.000457;0.001233;0.005692;0.010338;0.011192;0.017525;0.006971;0.006927;0.013561;0.00092
560;0.000451;0.001214;0.005666;0.010271;0.011252;0.017536;0.007034;0.006949;0.01365;0.000944
562;0.000445;0.001198;0.005645;0.010208;0.011313;0.017538;0.007085;0.006958;0.013738;0.000968
564;0.000438;0.001178;0.005614;0.010132;0.011367;0.017529;0.007126;0.006958;0.013824;0.000993
566;0.00043;0.001154;0.005572;0.010042;0.011415;0.017512;0.007158;0.00695;0.013909;0.001019
568;0.000419;0.001125;0.005513;0.009935;0.011452;0.017484;0.007176;0.006933;0.013993;0.001044
570;0.000407;0.00109;0.005436;0.009813;0.011478;0.017449;0.007178;0.006904;0.014074;0.00107
572;0.000394;0.001053;0.005347;0.009692;0.011496;0.017424;0.007164;0.006866;0.014153;0.001096
574;0.000377;0.001004;0.005223;0.009532;0.011491;0.017385;0.007137;0.006823;0.014229;0.001121
576;0.000358;0.00095;0.005075;0.009345;0.011468;0.017339;0.007101;0.00678;0.014303;0.001147
578;0.000337;0.000891;0.004901;0.009125;0.011425;0.017283;0.007062;0.006744;0.014374;0.001171
580;0.000315;0.000829;0.004703;0.008864;0.011359;0.017204;0.007019;0.006712;0.014443;0.001195
582;0.000291;0.000764;0.004483;0.008552;0.01127;0.017078;0.006968;0.006677;0.014508;0.001218
584;0.000269;0.000701;0.004251;0.008204;0.011162;0.016916;0.006914;0.006644;0.014572;0.00124
586;0.000247;0.000642;0.004017;0.007833;0.011041;0.016725;0.006863;0.006616;0.014634;0.001261
588;0.000226;0.000584;0.003772;0.007425;0.010898;0.016492;0.006811;0.006591;0.014693;0.001281
590;0.000204;0.000526;0.003507;0.006962;0.010722;0.016196;0.00675;0.006566;0.01475;0.001298
592;0.000185;0.000474;0.003251;0.0065;0.010536;0.015874;0.006688;0.006541;0.014805;0.001314
594;0.000166;0.000425;0.002989;0.006016;0.010324;0.015507;0.00662;0.00652;0.014858;0.001328
596;0.00015;0.000383;0.002755;0.005576;0.010119;0.015148;0.006554;0.006502;0.014913;0.001342
598;0.000137;0.000347;0.002548;0.00518;0.009922;0.014797;0.006488;0.006483;0.014968;0.001356
600;0.000126;0.000318;0.002375;0.004843;0.009749;0.014475;0.006424;0.006461;0.015026;0.001371
602;0.000118;0.000298;0.002248;0.004593;0.009621;0.014224;0.006373;0.006437;0.015088;0.00139
604;0.000112;0.000283;0.002151;0.004399;0.009526;0.01402;0.006326;0.006407;0.015153;0.001411
606;0.000109;0.000274;0.002096;0.004288;0.009486;0.013899;0.006293;0.006369;0.01522;0.001436
608;0.000107;0.000269;0.002062;0.004215;0.009474;0.013818;0.006266;0.006324;0.015289;0.001464
610;0.000106;0.000267;0.002049;0.00418;0.009492;0.01378;0.006251;0.006278;0.015359;0.001495
612;0.000106;0.000266;0.002045;0.004162;0.009524;0.013759;0.00624;0.00623;0.01543;0.001528
614;0.000105;0.000264;0.002036;0.004134;0.009546;0.013725;0.006225;0.00618;0.015499;0.00156
616;0.000105;0.000263;0.002031;0.004114;0.009574;0.013701;0.006211;0.006132;0.015568;0.001594
618;0.000104;0.000262;0.002021;0.004087;0.009595;0.013667;0.006193;0.006085;0.015636;0.001627
620;0.000103;0.00026;0.002006;0.004048;0.009606;0.013617;0.006167;0.006038;0.015702;0.001659
622;0.000102;0.000257;0.00199;0.004008;0.009614;0.013565;0.006138;0.005992;0.015768;0.001692
624;0.000101;0.000254;0.001972;0.003967;0.00962;0.01351;0.006106;0.005947;0.015833;0.001725
626;0.0001;0.000252;0.001955;0.003926;0.009624;0.013456;0.006072;0.005905;0.015898;0.001758
628;0.000099;0.000249;0.001939;0.003889;0.009631;0.013409;0.006044;0.005871;0.015962;0.001792
630;0.000098;0.000247;0.001922;0.003852;0.009635;0.013366;0.00602;0.005847;0.016026;0.001827
632;0.000097;0.000244;0.001903;0.00381;0.009634;0.013321;0.005999;0.005828;0.016088;0.001861
634;0.000096;0.000241;0.001884;0.00377;0.009633;0.013281;0.005983;0.005813;0.01615;0.001896
636;0.000095;0.000238;0.001864;0.003729;0.009629;0.013244;0.00597;0.005798;0.016212;0.001931
638;0.000094;0.000235;0.001843;0.003687;0.009622;0.013208;0.00596;0.005783;0.016273;0.001966
640;0.000093;0.000232;0.00182;0.00364;0.009611;0.013169;0.00595;0.005769;0.016333;0.002
642;0.000091;0.000228;0.001795;0.003592;0.009595;0.01313;0.005941;0.005756;0.016392;0.002035
644;0.00009;0.000224;0.001768;0.003541;0.009575;0.013086;0.005931;0.005747;0.016451;0.002069
646;0.000088;0.00022;0.001737;0.00348;0.009544;0.013025;0.005918;0.005747;0.016509;0.002102
648;0.000086;0.000215;0.001701;0.003408;0.009501;0.012939;0.005894;0.005744;0.016566;0.002133
650;0.000084;0.000209;0.001659;0.003321;0.009443;0.012819;0.005849;0.005728;0.016622;0.002161
652;0.000081;0.000202;0.001606;0.003209;0.009356;0.012645;0.005776;0.005688;0.016677;0.002183
654;0.000078;0.000194;0.001549;0.003088;0.009256;0.012435;0.005674;0.005611;0.016731;0.002202
656;0.000075;0.000186;0.001486;0.002952;0.009134;0.012175;0.005532;0.005485;0.016784;0.002215
658;0.000072;0.000178;0.001425;0.00282;0.009009;0.011895;0.005357;0.005311;0.016837;0.002225
660;0.000069;0.000172;0.001375;0.002706;0.008899;0.011616;0.005158;0.005094;0.016891;0.002235
662;0.000067;0.000166;0.001332;0.002605;0.008803;0.011346;0.004951;0.004856;0.016946;0.002247
664;0.000065;0.000163;0.001303;0.00253;0.008737;0.011115;0.004756;0.004621;0.017002;0.002263
666;0.000064;0.00016;0.00128;0.002466;0.008684;0.010902;0.004574;0.004401;0.017059;0.002281
668;0.000063;0.000158;0.001264;0.002417;0.008648;0.010724;0.004416;0.004209;0.017117;0.002304
670;0.000063;0.000157;0.00125;0.002377;0.008621;0.010577;0.004289;0.004055;0.017174;0.002328
672;0.000062;0.000155;0.001237;0.002342;0.008594;0.010458;0.004194;0.003943;0.01723;0.002355
674;0.000061;0.000154;0.001226;0.002317;0.008578;0.010381;0.004132;0.003872;0.017283;0.002384
676;0.000061;0.000152;0.001214;0.002295;0.00856;0.010334;0.004105;0.003844;0.017334;0.002415
678;0.00006;0.00015;0.001199;0.002274;0.008538;0.010321;0.004114;0.003865;0.017381;0.002446
680;0.000059;0.000148;0.001183;0.002256;0.008514;0.010343;0.004163;0.003943;0.017425;0.002478
682;0.000059;0.000145;0.001165;0.002242;0.008488;0.010406;0.00426;0.00409;0.017466;0.002512
684;0.000058;0.000143;0.001145;0.002229;0.00846;0.010503;0.004407;0.004316;0.017505;0.002548
686;0.000057;0.00014;0.001123;0.002215;0.008427;0.010631;0.004605;0.004627;0.017545;0.002587
688;0.000056;0.000136;0.0011;0.002198;0.008388;0.010763;0.004841;0.005021;0.017588;0.002627
690;0.000054;0.000132;0.001072;0.002167;0.00833;0.01086;0.00509;0.005482;0.017634;0.002665
692;0.000053;0.000128;0.001043;0.002129;0.008266;0.010926;0.005336;0.005977;0.017684;0.002704
694;0.000051;0.000124;0.00101;0.002079;0.008183;0.010941;0.005568;0.006498;0.017737;0.002739
696;0.000049;0.000119;0.000974;0.002017;0.008082;0.010904;0.005777;0.007033;0.017793;0.00277
698;0.000047;0.000114;0.000932;0.001937;0.007947;0.010789;0.005934;0.007543;0.017849;0.002789
700;0.000045;0.000108;0.000882;0.001838;0.007773;0.010588;0.006031;0.008037;0.017906;0.002794
702;0.000042;0.000101;0.000832;0.001735;0.007584;0.010343;0.006079;0.008522;0.017961;0.00279
704;0.00004;0.000095;0.000778;0.001623;0.007369;0.010041;0.006067;0.008998;0.018015;0.002773
706;0.000037;0.000088;0.000723;0.001507;0.007128;0.009692;0.005995;0.009476;0.018065;0.002743
708;0.000034;0.000081;0.000669;0.001391;0.00687;0.009309;0.005871;0.009966;0.018112;0.002701
710;0.000031;0.000075;0.000615;0.001277;0.006595;0.008898;0.0057;0.010466;0.018153;0.002647
712;0.000029;0.000069;0.000563;0.001166;0.00631;0.00847;0.005492;0.01097;0.018189;0.002582
714;0.000027;0.000063;0.000515;0.001063;0.006022;0.008042;0.00526;0.011466;0.018216;0.002511
716;0.000024;0.000058;0.000471;0.000967;0.005734;0.007615;0.005011;0.011935;0.018235;0.002433
718;0.000022;0.000053;0.00043;0.000879;0.00545;0.007197;0.004754;0.012362;0.018244;0.002352
720;0.00002;0.000048;0.000392;0.000798;0.005168;0.006787;0.00449;0.012735;0.018242;0.002265
722;0.000018;0.000044;0.000355;0.000718;0.004869;0.006357;0.004205;0.013041;0.018223;0.002168
724;0.000017;0.00004;0.000322;0.000648;0.004584;0.005951;0.00393;0.01328;0.01819;0.00207
726;0.000015;0.000036;0.000291;0.000582;0.004295;0.005546;0.003651;0.013433;0.018138;0.001966
728;0.000014;0.000033;0.000264;0.000525;0.00403;0.005178;0.003395;0.013513;0.018072;0.001868
730;0.000013;0.00003;0.000243;0.00048;0.003799;0.004863;0.003174;0.013546;0.018001;0.001781
732;0.000012;0.000028;0.000224;0.000441;0.00359;0.004579;0.002976;0.013533;0.017922;0.001701
734;0.000011;0.000026;0.00021;0.000413;0.00343;0.004363;0.002825;0.013519;0.017857;0.00164
736;0.000011;0.000025;0.0002;0.000394;0.003319;0.004215;0.002722;0.013525;0.017814;0.001601
738;0.00001;0.000024;0.000194;0.00038;0.003238;0.004108;0.002648;0.01354;0.017787;0.001575
740;0.00001;0.000024;0.00019;0.000372;0.003188;0.004042;0.002604;0.013578;0.017781;0.001562
742;0.00001;0.000023;0.000187;0.000366;0.003156;0.004;0.002576;0.013624;0.017786;0.001557
744;0.00001;0.000023;0.000185;0.000362;0.003132;0.003969;0.002556;0.013669;0.017797;0.001556
746;0.00001;0.000023;0.000184;0.00036;0.003121;0.003955;0.002547;0.013722;0.017817;0.001561
748;0.00001;0.000023;0.000184;0.000359;0.003114;0.003948;0.002543;0.013774;0.01784;0.001568
750;0.00001;0.000023;0.000183;0.000359;0.003113;0.003947;0.002543;0.013825;0.017867;0.001577
752;0.00001;0.000023;0.000184;0.00036;0.003117;0.003953;0.002549;0.013879;0.017898;0.001588
754;0.00001;0.000023;0.000184;0.000361;0.003123;0.003962;0.002556;0.01393;0.01793;0.001601
756;0.00001;0.000023;0.000185;0.000362;0.003134;0.003977;0.002568;0.013983;0.017965;0.001616
758;0.00001;0.000023;0.000186;0.000364;0.003145;0.003993;0.00258;0.014034;0.018;0.001631
760;0.00001;0.000023;0.000187;0.000366;0.003159;0.004013;0.002595;0.014086;0.018037;0.001648
762;0.00001;0.000024;0.000189;0.00037;0.00318;0.004042;0.002616;0.014145;0.018078;0.001668
764;0.00001;0.000024;0.000191;0.000374;0.003207;0.004079;0.002643;0.01421;0.018123;0.001691
766;0.00001;0.000024;0.000193;0.000379;0.003238;0.004121;0.002674;0.01428;0.018171;0.001716
768;0.000011;0.000025;0.000196;0.000385;0.003275;0.004171;0.002711;0.014356;0.018222;0.001745
770;0.000011;0.000025;0.000199;0.000392;0.003317;0.004228;0.002752;0.014437;0.018275;0.001777
772;0.000011;0.000026;0.000203;0.0004;0.003364;0.004293;0.002799;0.014524;0.018331;0.001812
774;0.000011;0.000026;0.000208;0.000409;0.003417;0.004364;0.002851;0.014615;0.018389;0.001849
776;0.000011;0.000027;0.000212;0.000419;0.003471;0.004438;0.002905;0.014707;0.018447;0.001888
778;0.000012;0.000027;0.000217;0.000429;0.003529;0.004515;0.002961;0.0148;0.018504;0.001929
780;0.000012;0.000028;0.000222;0.000439;0.00359;0.004599;0.003022;0.014896;0.018563;0.001973
782;0.000012;0.000029;0.000227;0.000451;0.003652;0.004684;0.003085;0.01499;0.01862;0.002017
784;0.000013;0.000029;0.000233;0.000462;0.003715;0.004768;0.003147;0.015082;0.018676;0.002062
786;0.000013;0.00003;0.000238;0.000473;0.003776;0.004852;0.003208;0.01517;0.018729;0.002106
788;0.000013;0.00003;0.000243;0.000483;0.00383;0.004925;0.003262;0.015246;0.018776;0.002147
790;0.000013;0.000031;0.000247;0.000493;0.003879;0.004993;0.003312;0.015315;0.018821;0.002185
792;0.000014;0.000031;0.000251;0.0005;0.00392;0.005048;0.003354;0.015372;0.01886;0.002219
794;0.000014;0.000032;0.000254;0.000507;0.003952;0.005092;0.003386;0.015416;0.018894;0.002247
796;0.000014;0.000032;0.000256;0.000511;0.003973;0.005121;0.003408;0.015449;0.018923;0.00227
798;0.000014;0.000032;0.000257;0.000513;0.003983;0.005134;0.003418;0.015466;0.018947;0.002285
800;0.000014;0.000032;0.000256;0.000512;0.003978;0.005128;0.003413;0.015466;0.018963;0.002292
802;0.000014;0.000032;0.000255;0.000508;0.003957;0.005099;0.003392;0.015447;0.018972;0.002288
804;0.000014;0.000032;0.000251;0.000501;0.003922;0.005052;0.003357;0.015411;0.018973;0.002277
806;0.000013;0.000031;0.000247;0.000492;0.00387;0.004981;0.003305;0.015354;0.018966;0.002254
808;0.000013;0.00003;0.000241;0.00048;0.003806;0.004895;0.003242;0.01528;0.018951;0.002224
810;0.000013;0.000029;0.000234;0.000466;0.003728;0.004789;0.003165;0.015186;0.018927;0.002185
812;0.000012;0.000028;0.000226;0.000449;0.003635;0.004665;0.003074;0.01507;0.018892;0.002136
814;0.000012;0.000027;0.000218;0.000431;0.003532;0.004527;0.002973;0.014936;0.018847;0.002081
816;0.000011;0.000026;0.000208;0.000411;0.003413;0.004367;0.002858;0.014772;0.018786;0.002015
818;0.000011;0.000025;0.000198;0.000389;0.003284;0.004195;0.002734;0.014586;0.018711;0.001943
820;0.00001;0.000024;0.000188;0.000369;0.003158;0.004028;0.002614;0.014395;0.01863;0.001872
822;0.00001;0.000022;0.000178;0.000348;0.003024;0.003851;0.002489;0.01418;0.018534;0.001796
824;0.000009;0.000021;0.000168;0.000328;0.002892;0.003677;0.002365;0.013956;0.018427;0.00172
826;0.000009;0.00002;0.000159;0.00031;0.002769;0.003515;0.002251;0.013735;0.018318;0.001649
828;0.000008;0.000019;0.000151;0.000294;0.002653;0.003362;0.002144;0.013515;0.018205;0.001582
830;0.000008;0.000018;0.000144;0.00028;0.002554;0.003232;0.002054;0.01332;0.018101;0.001526
832;0.000008;0.000017;0.000138;0.000268;0.002468;0.00312;0.001977;0.013145;0.018007;0.001477
834;0.000007;0.000017;0.000133;0.000258;0.002393;0.003024;0.001911;0.012991;0.017922;0.001436
836;0.000007;0.000016;0.000129;0.000249;0.002328;0.00294;0.001854;0.012851;0.017845;0.0014
838;0.000007;0.000016;0.000125;0.000241;0.002262;0.002855;0.001797;0.012709;0.017764;0.001364
840;0.000007;0.000015;0.000121;0.000233;0.002199;0.002774;0.001743;0.012568;0.017682;0.00133
842;0.000006;0.000015;0.000116;0.000224;0.002133;0.00269;0.001687;0.012417;0.017591;0.001294
844;0.000006;0.000014;0.000113;0.000217;0.002071;0.002611;0.001634;0.012269;0.017501;0.00126
846;0.000006;0.000014;0.000109;0.00021;0.002015;0.002539;0.001586;0.012133;0.017416;0.00123
848;0.000006;0.000013;0.000106;0.000203;0.001962;0.002471;0.001542;0.012;0.017331;0.0012
850;0.000006;0.000013;0.000103;0.000198;0.001915;0.002411;0.001502;0.011881;0.017255;0.001175
852;0.000006;0.000013;0.0001;0.000192;0.00187;0.002355;0.001466;0.011766;0.01718;0.001151
854;0.000005;0.000013;0.000098;0.000187;0.001829;0.002303;0.001431;0.011657;0.017108;0.001129
856;0.000005;0.000012;0.000096;0.000183;0.001791;0.002255;0.0014;0.011556;0.017041;0.001109
858;0.000005;0.000012;0.000094;0.000179;0.001757;0.002211;0.001372;0.01146;0.016977;0.00109
860;0.000005;0.000012;0.000092;0.000176;0.001726;0.002172;0.001346;0.011375;0.01692;0.001074
862;0.000005;0.000012;0.00009;0.000172;0.001698;0.002136;0.001323;0.011297;0.016868;0.001059
864;0.000005;0.000011;0.000089;0.000169;0.00167;0.002102;0.001301;0.01122;0.016816;0.001045
866;0.000005;0.000011;0.000087;0.000166;0.001645;0.00207;0.001281;0.011147;0.016766;0.001032
868;0.000005;0.000011;0.000086;0.000164;0.00162;0.002039;0.001261;0.011075;0.016717;0.001019
870;0.000005;0.000011;0.000084;0.000161;0.001595;0.002007;0.00124;0.011002;0.016665;0.001005
872;0.000005;0.000011;0.000083;0.000158;0.001571;0.001976;0.001221;0.010928;0.016613;0.000992
874;0.000005;0.00001;0.000082;0.000156;0.001546;0.001945;0.001201;0.010855;0.01656;0.000979
876;0.000005;0.00001;0.00008;0.000153;0.001522;0.001915;0.001182;0.010781;0.016507;0.000967
878;0.000004;0.00001;0.000079;0.00015;0.001499;0.001886;0.001163;0.010709;0.016454;0.000954
880;0.000004;0.00001;0.000078;0.000148;0.001477;0.001858;0.001145;0.010638;0.016402;0.000942
882;0.000004;0.00001;0.000077;0.000146;0.001455;0.00183;0.001127;0.010568;0.016349;0.00093
884;0.000004;0.00001;0.000076;0.000143;0.001434;0.001803;0.00111;0.010499;0.016297;0.000919
886;0.000004;0.00001;0.000074;0.000141;0.001412;0.001776;0.001093;0.010428;0.016243;0.000907
888;0.000004;0.000009;0.000073;0.000139;0.001391;0.001749;0.001075;0.010354;0.016187;0.000895
890;0.000004;0.000009;0.000072;0.000136;0.001368;0.00172;0.001057;0.010278;0.016127;0.000883
892;0.000004;0.000009;0.000071;0.000134;0.001345;0.001691;0.001039;0.010197;0.016063;0.000869
894;0.000004;0.000009;0.000069;0.000132;0.001322;0.001661;0.00102;0.010113;0.015995;0.000856
896;0.000004;0.000009;0.000068;0.000129;0.001301;0.001635;0.001003;0.010037;0.015934;0.000844
898;0.000004;0.000009;0.000067;0.000127;0.001278;0.001606;0.000984;0.009954;0.015865;0.000831
900;0.000004;0.000008;0.000066;0.000125;0.001257;0.00158;0.000968;0.009878;0.015803;0.000819
"
  
  # Descrizioni testuali delle classi
  descriptions <- c(
    "1"  = "Extremely clear and oligotrophic indigo-blue waters with high reflectance in the short visible wavelengths",
    "2"  = "Blue waters with a low presence of detritus and CDOM",
    "3a" = "Turquoise waters with a moderate presence of phytoplankton, detritus, and CDOM.",
    "3b" = "Turquoise waters with a moderate presence of phytoplankton, detritus, and CDOM (characterized by a strong scattering and little absorbing particles like in the case of Coccolithophore blooms)",
    "4a" = "Greenish water found in coastal and inland environments, with high biomass (reflectance in short wavelengths is usually depressed by the absorption of particles and CDOM)",
    "4b" = "Greenish water found in coastal and inland environments, with high biomass (exhibits phytoplankton blooms with high scattering coefficients, e.g., Coccolithophore bloom)",
    "5a" = "Green eutrophic water, with significantly high phytoplankton biomass (exhibits a bimodal reflectance shape with typical peaks at ~560 and ~709 nm)",
    "5b" = "Green hyper-eutrophic water, with significantly high phytoplankton biomass (exhibits a reflectance plateau in the Near Infrared Region)",
    "6"  = "Bright brown water with high detritus concentrations, which has a high reflectance determined by scattering",
    "7"  = "Dark brown to black water with very high CDOM concentration, which has low reflectance in the entire visible range and is dominated by absorption"
  )
  
  ref_means <- read.csv(text = owt_means_string, sep = ";", check.names = FALSE)
  ref_sds   <- read.csv(text = owt_sds_string, sep = ";", check.names = FALSE)
  
  # Identify the wavelengths common between the WISPstation and the OWT references
  nm_cols <- grep("^nm_", colnames(data), value = TRUE)
  wv_data <- gsub("nm_", "", nm_cols)
  wv_ref  <- as.character(ref_means$wavelen)
  wv_common_num  <- intersect(wv_data, wv_ref)
  wv_common_cols <- paste0("nm_", wv_common_num)
  
  if (length(wv_common_num) < 10) {
    warning("Too few bands in common between the data and the OWT references")
  }
  
  # Matrix extraction
  m_obs <- as.matrix(data[, wv_common_cols])
  
  # Filtering only for wavelengths present in the data
  df_means_sub <- ref_means[ref_means$wavelen %in% wv_common_num, ]
  df_sds_sub   <- ref_sds[ref_sds$wavelen %in% wv_common_num, ]
  
  # Transpose
  m_ref_mu <- t(as.matrix(df_means_sub[, -1]))
  m_ref_sd <- t(as.matrix(df_sds_sub[, -1]))
  owt_labels <- rownames(m_ref_mu)
  
  # Log-Likelihood calculation assuming a normal distribution for each band
  n_obs <- nrow(m_obs)
  n_classes <- length(owt_labels)
  log_likelihood_matrix <- matrix(NA, nrow = n_obs, ncol = n_classes)
  colnames(log_likelihood_matrix) <- owt_labels
  
  for (i in 1:n_classes) {
    mu <- m_ref_mu[i, ]
    sigma <- m_ref_sd[i, ]
    sigma[sigma <= 0] <- min(sigma[sigma > 0]) * 0.1 
    
    # Calculation for each observation 
    diff_sq <- t(apply(m_obs, 1, function(x) ((x - mu) / sigma)^2))
    log_const <- sum(log(sigma * sqrt(2 * pi)))
    log_likelihood_matrix[, i] <- rowSums(-0.5 * diff_sq) - log_const
  }
  
  # Transformation from Log-Likelihood to Membership (0-1)
  n_bands <- length(wv_common_num)
  scaled_log_l <- log_likelihood_matrix / n_bands
  
  adj_log_l <- t(apply(scaled_log_l, 1, function(x) x - max(x)))
  exp_l <- exp(adj_log_l)
  membership_matrix <- exp_l / rowSums(exp_l)
  
  # Results assignment
  max_idx <- apply(membership_matrix, 1, which.max)
  selected_classes <- owt_labels[max_idx]
  
  data$OWT_class       <- selected_classes
  data$OWT_description <- descriptions[selected_classes]
  data$OWT_score       <- round(apply(membership_matrix, 1, max), 3)
  
  best_z2 <- sapply(1:n_obs, function(j) {
    class_idx <- max_idx[j]
    z2 <- ((m_obs[j,] - m_ref_mu[class_idx,]) / m_ref_sd[class_idx,])^2
    mean(z2)
  })
  data$OWT_z_dist <- round(sqrt(best_z2), 3)
  
  return(data)
}



#' Create a plot of reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function return a plotly of each spectral signature measured by a
#' WISPstation.
#' @param data A `tibble` obtained by any of the functions provided by this
#' package: `wisp_get_reflectance_data()`, or after QC and SR removal operations.
#' @param legend_TSM A `logical`. If `TRUE`, the plot legend includes the `TSM`
#' values. Default is `TRUE`.
#' @param legend_Chla A `logical`. If `TRUE`, the plot legend includes the
#' `Chla` values. Default is `TRUE`.
#' @param legend_Kd A `logical`. If `TRUE`, the plot legend includes the `Kd`
#' values. Default is `TRUE`.
#' @param legend_cpc A `logical`. If `TRUE`, the plot legend includes the `cpc`
#' values. Default is `TRUE`.
#' @param legend_scatt A `logical`. If `TRUE`, the plot legend includes the `scattering`
#' values. Default is `FALSE`.
#' @param legend_ratio A `logical`. If `TRUE`, the plot legend includes the `ratio`
#' values. Default is `FALSE`.
#' @param legend_novoa_SPM A `logical`. If `TRUE`, the plot legend includes 
#' the `Novoa_SPM`values. Default is `FALSE`.
#' @param legend_novoa_TUR A `logical`. If `TRUE`, the plot legend includes 
#' the `Novoa_TUR`values. Default is `FALSE`.
#' @param legend_jiang_TSS A `logical`. If `TRUE`, the plot legend includes 
#' the `Jiang_TSS`values. Default is `FALSE`.
#' @param legend_gons_CHL A `logical`. If `TRUE`, the plot legend includes 
#' the `Gons_CHL`values. Default is `FALSE`.
#' @param legend_gons740_CHL A `logical`. If `TRUE`, the plot legend includes 
#' the `Gons740_CHL`values. Default is `FALSE`.
#' @param legend_NDCI A `logical`. If `TRUE`, the plot legend includes 
#' the `NDCI`values. Default is `FALSE`.
#' @param legend_mishra_CHL A `logical`. If `TRUE`, the plot legend includes 
#' the `Mishra_CHL`values. Default is `FALSE`.
#' @param legend_hue_angle A `logical`. If `TRUE`, the plot legend includes 
#' the `Hue_Angle`values. Default is `FALSE`.
#' @param legend_dom_wavelength A `logical`. If `TRUE`, the plot legend includes 
#' the `Dominant_Wavelength`values. Default is `FALSE`.
#' @param legend_OWT_class A `logical`. If `TRUE`, the plot legend includes 
#' the `OWT_class`. Default is `FALSE`.
#' @param legend_OWT_score A `logical`. If `TRUE`, the plot legend includes 
#' the `OWT_score` (membership grade). Default is `FALSE`.
#' @param legend_OWT_z_dist A `logical`. If `TRUE`, the plot legend includes 
#' the `OWT_z_dist` (statistical distance). Default is `FALSE`.
#' @return an interactive plot showing the spectral signatures of the
#' reflectance data.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_longer
#' @importFrom viridis viridis
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs theme_minimal
#' @importFrom plotly ggplotly
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' wisp_plot_reflectance_data(
#'   data = reflect_data_sr,
#'   legend_TSM = TRUE,
#'   legend_Chla = TRUE,
#'   legend_Kd = TRUE,
#'   legend_cpc = TRUE,
#'   legend_scatt = FALSE,
#'   legend_ratio = FALSE,
#'   legend_novoa_SPM = FALSE,
#'   legend_novoa_TUR = FALSE,
#'   legend_jiang_TSS = FALSE,
#'   legend_gons_CHL  = FALSE,
#'   legend_gons740_CHL = FALSE,
#'   legend_NDCI = FALSE,
#'   legend_mishra_CHL = FALSE,
#'   legend_hue_angle = FALSE, 
#'   legend_dom_wavelength = FALSE,
#'   legend_OWT_class = FALSE,
#'   legend_OWT_score = FALSE,
#'   legend_OWT_z_dist = FALSE  
#' )
#' }
#' ## End (Not run)
#'
### wisp_plot_reflectance_data
wisp_plot_reflectance_data <- function(
    data,
    legend_TSM            = TRUE,
    legend_Chla           = TRUE,
    legend_Kd             = TRUE,
    legend_cpc            = TRUE,
    legend_scatt          = FALSE,
    legend_ratio          = FALSE,
    legend_novoa_SPM      = FALSE,
    legend_novoa_TUR      = FALSE,
    legend_jiang_TSS      = FALSE,
    legend_gons_CHL       = FALSE,
    legend_gons740_CHL    = FALSE,
    legend_NDCI           = FALSE,
    legend_mishra_CHL     = FALSE,
    legend_hue_angle      = FALSE, 
    legend_dom_wavelength = FALSE,
    legend_OWT_class      = FALSE,
    legend_OWT_score      = FALSE,
    legend_OWT_z_dist     = FALSE
) {
  
  # Convert columns with ‘units’ to numeric
  units_cols <- names(data)[sapply(data, function(x) inherits(x, "units"))]
  if(length(units_cols) > 0){
    for(col in units_cols){
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  # Creation of the 'products_info' column
  data$products_info <- mapply(
    function(tsm, chla, kd, cpc, scatt, ratio, novoa_spm, novoa_tur, jiang_tss, 
             gons_chl, gons740_chl, ndci, mishra_chl, hue, dom_wv, owt_c, owt_s, owt_z) {
      paste(
        c(
          if (legend_TSM && !is.na(tsm)) paste("<b>TSM [g/m3]:</b>", tsm),
          if (legend_Chla && !is.na(chla)) paste("<b>Chla [mg/m3]:</b>", chla),
          if (legend_Kd && !is.na(kd)) paste("<b>Kd [1/m]:</b>", kd),
          if (legend_cpc && !is.na(cpc)) paste("<b>Cpc [mg/m3]:</b>", cpc),
          if (legend_scatt && !is.na(scatt)) paste("<b>Scatt [1/sr]:</b>", scatt),
          if (legend_ratio && !is.na(ratio)) paste("<b>Ratio:</b>", ratio),
          if (legend_novoa_SPM && !is.na(novoa_spm)) paste("<b>Novoa_SPM [g/m3]:</b>", novoa_spm),
          if (legend_novoa_TUR && !is.na(novoa_tur)) paste("<b>Novoa_TUR [NTU]:</b>", novoa_tur),
          if (legend_jiang_TSS && !is.na(jiang_tss)) paste("<b>Jiang_TSS [g/m3]:</b>", jiang_tss),
          if (legend_gons_CHL && !is.na(gons_chl)) paste("<b>Gons_CHL [mg/m3]:</b>", gons_chl),
          if (legend_gons740_CHL && !is.na(gons740_chl)) paste("<b>Gons740_CHL [mg/m3]:</b>", gons740_chl),
          if (legend_NDCI && !is.na(ndci)) paste("<b>NDCI:</b>", ndci),
          if (legend_mishra_CHL && !is.na(mishra_chl)) paste("<b>Mishra_CHL [mg/m3]:</b>", mishra_chl),
          if (legend_hue_angle && !is.na(hue)) paste("<b>Hue_Angle [°]:</b>", hue),
          if (legend_dom_wavelength && !is.na(dom_wv)) paste("<b>Dom_Wave [nm]:</b>", dom_wv),
          if (legend_OWT_class && !is.na(owt_c)) paste("<b>OWT Class:</b>", owt_c),
          if (legend_OWT_score && !is.na(owt_s)) paste("<b>OWT Score:</b>", owt_s),
          if (legend_OWT_z_dist && !is.na(owt_z)) paste("<b>OWT Z-Dist:</b>", owt_z)
        ),
        collapse = "<br>"
      )
    },
    tsm         = if ("waterquality.tsm"     %in% names(data)) data$waterquality.tsm else NA,
    chla        = if ("waterquality.chla"    %in% names(data)) data$waterquality.chla else NA,
    kd          = if ("waterquality.kd"      %in% names(data)) data$waterquality.kd else NA,
    cpc         = if ("waterquality.cpc"     %in% names(data)) data$waterquality.cpc else NA,
    scatt       = if ("scattering.peak"      %in% names(data)) data$scattering.peak else NA,
    ratio       = if ("band.ratio"           %in% names(data)) data$band.ratio else NA,
    novoa_spm   = if ("Novoa.SPM"            %in% names(data)) data$Novoa.SPM else NA,
    novoa_tur   = if ("Novoa.TUR"            %in% names(data)) data$Novoa.TUR else NA,
    jiang_tss   = if ("Jiang.TSS"            %in% names(data)) data$Jiang.TSS else NA,
    gons_chl    = if ("Gons.CHL"             %in% names(data)) data$Gons.CHL else NA,
    gons740_chl = if ("Gons740.CHL"          %in% names(data)) data$Gons740.CHL else NA,
    ndci        = if ("NDCI"                 %in% names(data)) data$NDCI else NA,
    mishra_chl  = if ("Mishra.CHL"           %in% names(data)) data$Mishra.CHL else NA,
    hue         = if ("hue_angle"            %in% names(data)) data$hue_angle else NA,      
    dom_wv      = if ("dominant_wavelength"  %in% names(data)) data$dominant_wavelength else NA,
    owt_c       = if ("OWT_class"            %in% names(data)) data$OWT_class else NA,
    owt_s       = if ("OWT_score"            %in% names(data)) data$OWT_score else NA,
    owt_z       = if ("OWT_z_dist"           %in% names(data)) data$OWT_z_dist else NA
  )
  
  instr_name <- if("instrument.name" %in% names(data)) data$instrument.name[1] else "Unknown"
  
  # Data preparation
  nm_cols <- grep("^nm_", names(data), value = TRUE)
  data_2 <- tidyr::pivot_longer(
    data[, c("measurement.date", "products_info", nm_cols)],
    cols = dplyr::all_of(nm_cols),   
    names_to = "wavelength",
    values_to = "Rrs"
  )
  data_2$wavelength <- as.numeric(sub("nm_", "", data_2$wavelength))
  
  # Legend on the right
  data_2$color_group <- paste0(
    "<b>Date:</b> ", substr(data_2$measurement.date, 1, 10),
    "<br><b>Time [UTC]:</b> ", substr(data_2$measurement.date, 12, 19),
    "<br>", data_2$products_info
  )
  
  # Interactive legend (Tooltip)
  data_2$tooltip_text <- paste0(
    data_2$color_group, 
    "<br><br>", 
    "<b>Wavelength:</b> ", data_2$wavelength, " nm<br>",
    "<b>Rrs:</b> ", round(data_2$Rrs, 6), " [1/sr]"
  )
  
  num_colors <- length(unique(data_2$color_group))
  color_palette <- viridis::viridis(num_colors)
  
  # Plot generation
  p <- ggplot2::ggplot(data_2, ggplot2::aes(x=wavelength, y=Rrs, color=color_group, text=tooltip_text)) +
    ggplot2::geom_line(ggplot2::aes(group = measurement.date)) + 
    ggplot2::scale_color_manual(values=color_palette) +
    ggplot2::labs(
      title = paste0("Acquired by: ", instr_name),
      x = "Wavelength [nm]",
      y = "Rrs [1/sr]",
      color = "Time of acquisition"
    ) +
    ggplot2::theme_minimal()
  
  plotly::ggplotly(p, tooltip = "text")
}


#' Comparison plot of Raw vs QC vs SR reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function creates an interactive side-by-side comparison of different 
#' processing levels of WISPstation reflectance data using plotly.
#' @param raw_data A `tibble`. The original data obtained by `wisp_get_reflectance_data()`.
#' @param qc_data A `tibble`. The data after `wisp_qc_reflectance_data()` operations. 
#' Default is `NULL`.
#' @param sr_data A `tibble`. The data after `wisp_sr_reflectance_data()` operations. 
#' Default is `NULL`.
#' @param raw_args A `list` of arguments to be passed to `wisp_plot_reflectance_data` 
#' for the raw data plot (legend). Default is `NULL`.
#' @param qc_args A `list` of arguments to be passed to `wisp_plot_reflectance_data` 
#' for the QC data plot (legend). Default is `NULL`.
#' @param sr_args A `list` of arguments to be passed to `wisp_plot_reflectance_data` 
#' for the SR data plot (legend). Default is `NULL`.
#' @return A `plotly` subplot object comparing the spectral signatures. If only 
#' `raw_data` is provided or valid, a single plot is returned.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom plotly subplot layout
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' custom_raw <- list(legend_TSM = FALSE, legend_Chla = FALSE)
#' custom_qc <- list(legend_TSM = TRUE, legend_Chla = TRUE, legend_Kd = FALSE)
#' custom_sr <- list(legend_TSM = TRUE, legend_mishra_CHL = FALSE)
#'
#' wisp_plot_comparison(
#'   raw_data = reflect_data,
#'   qc_data  = reflect_data_qc,
#'   sr_data  = reflect_data_sr,
#'   raw_args = custom_raw,
#'   qc_args  = custom_qc,
#'   sr_args  = custom_sr
#' )
#' print(fig_comparison)
#' }
#' ## End (Not run)
#'
### wisp_plot_comparison
wisp_plot_comparison <- function(
    raw_data, 
    qc_data = NULL, 
    sr_data = NULL,
    raw_args = NULL,
    qc_args  = NULL,
    sr_args  = NULL
) {
  
  # Default settings for displaying the legend
  default_raw <- list(
    legend_TSM = TRUE, legend_Chla = TRUE, legend_Kd = TRUE, legend_cpc = TRUE)
  
  default_derived <- list(
    legend_TSM = TRUE, legend_Chla = TRUE, legend_Kd = TRUE, legend_cpc = TRUE,
    legend_scatt = TRUE, legend_ratio = TRUE, legend_novoa_SPM = TRUE,
    legend_novoa_TUR = TRUE, legend_jiang_TSS = TRUE, legend_gons_CHL = TRUE,
    legend_gons740_CHL = TRUE, legend_NDCI = TRUE, legend_mishra_CHL = TRUE,
    legend_OWT_class = TRUE, legend_OWT_score = TRUE, legend_OWT_z_dist = TRUE
  )
  
  # Assigning parameters
  r_params  <- if (is.null(raw_args)) default_raw else raw_args
  qc_params <- if (is.null(qc_args)) default_derived else qc_args
  sr_params <- if (is.null(sr_args)) default_derived else sr_args
  
  # Generation of individual plots
  fig1 <- do.call(wisp_plot_reflectance_data, c(list(data = raw_data), r_params))
  
  fig2 <- if (!is.null(qc_data) && nrow(qc_data) > 0) {
    do.call(wisp_plot_reflectance_data, c(list(data = qc_data), qc_params))
  } else NULL
  
  fig3 <- if (!is.null(sr_data) && nrow(sr_data) > 0) {
    do.call(wisp_plot_reflectance_data, c(list(data = sr_data), sr_params))
  } else NULL
  
  # Assembly of plots
  plot_list <- Filter(Negate(is.null), list(fig1, fig2, fig3))
  
  if (length(plot_list) == 1) {
    message("\n----\n⚠️ The only valid plot is the one referring to RAW.\n----\n")
  }
  
  plot_title <- if (length(plot_list) == 3) {
    "<b>Reflectance comparison: Raw vs QC vs SR<b>"
  } else {
    "<b>Reflectance: Raw<b>"
  }
  
  final_plot <- if (length(plot_list) > 1) {
    plotly::subplot(plot_list, nrows = 1, shareX = TRUE, shareY = TRUE)
  } else {
    fig1
  }
  
  # Layout and axis titles
  final_plot <- final_plot |>
    plotly::layout(
      title = plot_title,
      xaxis = list(title = "<b>Wavelength [nm]<b>", titlefont = list(size = 14)),
      yaxis = list(title = "<b>Rrs [1/sr]<b>", titlefont = list(size = 14))
    )
  
  if (length(plot_list) >= 2) {
    final_plot <- final_plot |> 
      plotly::layout(xaxis2 = list(title = "<b>Wavelength [nm]<b>", titlefont = list(size = 14)))
  }
  if (length(plot_list) >= 3) {
    final_plot <- final_plot |> 
      plotly::layout(xaxis3 = list(title = "<b>Wavelength [nm]<b>", titlefont = list(size = 14)))
  }
  
  return(final_plot)
}

#' Creates a temporal trend plot of one or more water quality parameters
#' @description `r lifecycle::badge("experimental")`
#' This function creates an interactive plot of the time trend of one or
#' more water quality parameters associated with spectral signatures 
#' @param data A `tibble` containing water quality parameters and spectral
#' signatures. 
#' @param params A character vector specifying which parameters to plot.
#' Default is `c("TSM", "Chla")`.
#' @param datetime_col A `character`. Name of the column with datetime 
#' values. Default is `"measurement.date"`.
#' @param instrument_col A `character`. Name of the column with instrument 
#' identifiers. Default is `"instrument.name"`.
#' @param aggregate A `character` specifying whether to aggregate data.
#' Options are:
#' \itemize{
#'   \item \code{"none"}: Plots all available values (requires data for only one day).
#'   \item \code{"daily_mean"}: Calculates and plots the daily average, including a 
#'   ribbon for the Standard Deviation (SD) (requires data for multiple days).
#'   \item \code{"daily_median"}: Calculates and plots the daily median 
#'   (requires data for multiple days).} Default is `"none"`.
#' @param merge_plot A `logical`. If \code{TRUE}, parameters that share the same 
#' unit of measurement will be merged into a single plot. The function will 
#' throw an error if no common units are found among the requested \code{params}. 
#' Default is \code{FALSE}.
#' @param na.rm A `logical`. If `TRUE`, NA values are ignored during aggregation. 
#' Default is `TRUE`.
#' @param colors A `character` vector of colors for each parameter. Default uses 
#' `viridis` palette.
#' @param title A `character`. Optional title for the plot. Default is `NULL`.
#' @param return_long_df A `logical`. If \code{TRUE}, the function returns the 
#' long format dataframe used for plotting instead of the \code{plotly} object. 
#' Default is \code{FALSE}.
#' @return An interactive `plotly` object showing the temporal trend of the 
#' selected parameters, with optional ribbons for standard deviation.
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, PhD \email{nicola.ghirardi@@cnr.it}
#' @importFrom plotly ggplotly
#' @importFrom dplyr group_by reframe summarise
#' @importFrom lubridate as_datetime
#' @importFrom viridis viridis
#' @importFrom stats sd setNames median
#' @importFrom units drop_units
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon scale_color_manual 
#' scale_fill_manual facet_wrap labs theme_minimal theme element_text element_blank 
#' as_labeller scale_x_datetime
#' @export
#' @examples
#' # Example usage
#' \dontrun{
#' # Standard plot with facets for each parameter
#' fig_trend <- wisp_trend_plot(
#'    data = reflect_data_sr,
#'    params = c("TSM", "Chla"),
#'    aggregate = "none",
#'    merge_plot = FALSE
#' )
#' print(fig_trend)
#' 
#' # Merged plot for parameters with common units 
#' fig_merged <- wisp_trend_plot(
#'    data = reflect_data_sr,
#'    params = c("TSM", "Novoa_SPM"),
#'    aggregate = "daily_mean",
#'    merge_plot = TRUE
#' )
#' print(fig_trend)
#' }
wisp_trend_plot <- function(
    data,
    params = c("TSM", "Chla"),
    datetime_col = "measurement.date",
    instrument_col = "instrument.name",
    aggregate = c("none", "daily_mean", "daily_median"),
    merge_plot = FALSE,
    na.rm = TRUE,
    colors = NULL,
    title = NULL,
    return_long_df = FALSE
) {
  aggregate <- match.arg(aggregate)
  
  if (is.null(data) || nrow(data) == 0) stop("⚠️ The ‘data’ dataset is empty or NULL.")
  if (!datetime_col %in% names(data)) stop("⚠️ Datetime column not found: ", datetime_col)
  
  mapping <- list(
    TSM          = "waterquality.tsm",
    Chla         = "waterquality.chla",
    Kd           = "waterquality.kd",
    cpc          = "waterquality.cpc",
    scatt        = "scattering.peak",
    ratio        = "band.ratio",
    Novoa_SPM    = "Novoa.SPM",
    Novoa_TUR    = "Novoa.TUR",
    Jiang_TSS    = "Jiang.TSS",
    Gons_CHL     = "Gons.CHL",
    Gons740_CHL  = "Gons740.CHL",
    NDCI         = "NDCI",
    Mishra_CHL   = "Mishra.CHL",
    Hue_Angle    = "hue_angle",           
    Dom_Wave     = "dominant_wavelength"  
  )
  
  # Mapping for grouping units of measurement
  unit_groups_map <- c(
    TSM = "[g/m3]", Novoa_SPM = "[g/m3]", Jiang_TSS = "[g/m3]",
    Chla = "[mg/m3]", cpc = "[mg/m3]", Gons_CHL = "[mg/m3]", Gons740_CHL = "[mg/m3]", Mishra_CHL = "[mg/m3]",
    Kd = "[1/m]", scatt = "[1/sr]", Novoa_TUR = "[NTU]", ratio = "ratio", NDCI = "NDCI",
    Hue_Angle = "[deg]", Dom_Wave = "[nm]"
  )
  
  # 1. Available parameters
  available_params <- names(mapping)
  message(
    "\n----------------------------------------------------------------------\n",
    "Parameters available (`params`): ",
    paste(available_params, collapse = ", ")
  )
  
  # Available aggregation methods
  message(
    "\nAggregation methods (`aggregate`):\n",
    " - 'none': Plot all available values (requires only one day in `wisp_get_reflectance_data()`).\n",
    " - 'daily_mean': Plot the daily average and standard deviation (requires multiple days in `wisp_get_reflectance_data()`).\n",
    " - 'daily_median': Plot the daily median (requires multiple days in `wisp_get_reflectance_data()`)\n",
    "----------------------------------------------------------------------\n"
  )
  
  units_mapping <- c(
    TSM         = "TSM [g/m3]",
    Chla        = "Chla [mg/m3]",
    Kd          = "Kd [1/m]",
    cpc         = "cpc [mg/m3]",
    scatt       = "scatt [1/sr]",
    ratio       = "ratio",
    Novoa_SPM   = "Novoa_SPM [g/m3]",
    Novoa_TUR   = "Novoa_TUR [NTU]",
    Jiang_TSS   = "Jiang.TSS [g/m3]",
    Gons_CHL    = "Gons.CHL [mg/m3]",
    Gons740_CHL = "Gons740.CHL [mg/m3]",
    NDCI        = "NDCI",
    Mishra_CHL  = "Mishra_CHL [mg/m3]",
    Hue_Angle   = "Hue Angle [deg]",      
    Dom_Wave    = "Dom. Wavelength [nm]" 
  )
  
  requested <- unique(params)
  valid <- requested[requested %in% names(mapping)]
  if (length(valid) == 0) stop("⚠️ No valid parameters required.")
  if (merge_plot) {
    relevant_units <- unit_groups_map[valid]
    if (length(unique(relevant_units)) == length(relevant_units)) {
      stop("⚠️ Please note: you are requesting to merge plots with parameters that do not have any common units of measurement. Try changing the 'params' or set 'merge_plot = FALSE'.")
    }
  }
  cols <- unlist(mapping[valid])
  present <- cols %in% names(data)
  if (!all(present)) warning("⚠️ Some parameters not present in the dataset: ", paste(valid[!present], collapse = ", "))
  cols <- cols[present]
  valid <- valid[present]
  
  drop_units_safe <- function(x) {
    if (inherits(x, "units")) as.numeric(units::drop_units(x)) else as.numeric(x)
  }
  
  datetime <- lubridate::as_datetime(data[[datetime_col]])
  dates_vec <- as.Date(datetime)
  unique_dates <- unique(dates_vec[!is.na(dates_vec)])
  n_unique_dates <- length(unique_dates)
  
  if (aggregate == "none" && n_unique_dates > 1) {
    stop("⚠️ Please note: you are requesting multiple days. Try changing the 'aggregate' or requesting a single day.")
  }
  if (aggregate %in% c("daily_mean", "daily_median") && n_unique_dates == 1) {
    stop("⚠️ Please note: you are requesting only one day. Try changing the 'aggregate' or requesting multiple days. 
     If, on the other hand, you requested multiple days, it means that QC removed all the spectral signatures in some of them.")
  }
  
  requested_day_label <- if (aggregate == "none" && n_unique_dates == 1) format(unique_dates, "%Y-%m-%d") else NULL
  instrument <- if (instrument_col %in% names(data)) as.character(data[[instrument_col]]) else rep(NA_character_, nrow(data))
  
  # Creating long_df with tooltip
  long_df <- data.frame(stringsAsFactors = FALSE)
  for (i in seq_along(valid)) {
    param <- valid[i]
    col <- cols[i]
    values <- drop_units_safe(data[[col]])
    tmp <- data.frame(
      datetime = datetime,
      instrument = instrument,
      param = param,
      unit_group = unit_groups_map[param],
      value = values,
      row.names = NULL, 
      stringsAsFactors = FALSE
    )
    
    val_rounded <- if(param == "Dom_Wave") round(tmp$value, 1) else round(tmp$value, 2)
    nsmall_val  <- if(param == "Dom_Wave") 1 else 2
    
    tmp$main_text <- paste0(
      "Date: ", format(tmp$datetime, "%Y-%m-%d"),
      "<br>Time: ", format(tmp$datetime, "%H:%M:%S"),
      "<br>", units_mapping[param], ": ", format(val_rounded, nsmall = nsmall_val)
    )
    long_df <- rbind(long_df, tmp)
  }
  
  long_df <- long_df[!is.na(long_df$value), ]
  if (nrow(long_df) == 0) stop("⚠️ No valid value after filtering NA.")
  
  # Aggregation if requested
  if (aggregate != "none") {
    long_df$date <- as.Date(long_df$datetime)
    grouped <- dplyr::group_by(long_df, param, unit_group, date)
    
    if (aggregate == "daily_mean") {
      long_df <- dplyr::reframe(
        grouped,
        mean_value = mean(value, na.rm = na.rm),
        sd_value   = stats::sd(value, na.rm = na.rm),
        main_text  = paste0(
          "Date: ", date,
          "<br>Param: ", param,
          "<br>Mean: ", round(mean(value, na.rm = na.rm), 2),
          " ± SD: ", round(stats::sd(value, na.rm = na.rm), 2)
        )
      )
    } else if (aggregate == "daily_median") {
      long_df <- dplyr::reframe(
        grouped,
        mean_value = stats::median(value, na.rm = na.rm),
        sd_value   = NA_real_,
        main_text  = paste0(
          "Date: ", date,
          "<br>Param: ", param,
          "<br>Median: ", round(stats::median(value, na.rm = na.rm), 2)
        )
      )
    }
    long_df$datetime <- as.POSIXct(long_df$date)
  } else {
    long_df$mean_value <- long_df$value
    long_df$sd_value <- NA_real_
  }
  
  if (return_long_df) return(long_df)
  
  # Colors and factor parameters
  params_unique <- as.character(unique(long_df$param))
  long_df$param <- factor(long_df$param, levels=params_unique)
  if (is.null(colors)) colors <- viridis::viridis(length(params_unique))
  colors_map <- stats::setNames(colors[seq_along(params_unique)], params_unique)
  legend_labels <- units_mapping[levels(long_df$param)]
  
  # Dynamic facet title logic (Merge vs Parameter)
  if (merge_plot) {
    df_grouped <- dplyr::group_by(long_df, unit_group)
    facet_titles <- dplyr::summarise(
      df_grouped, 
      title_str = paste0(paste(unique(as.character(param)), collapse = " vs "), " ", unique(unit_group)), 
      .groups = "drop"
    )
    long_df$facet_var <- long_df$unit_group
    facet_labeller <- ggplot2::as_labeller(stats::setNames(facet_titles$title_str, facet_titles$unit_group))
  } else {
    long_df$facet_var <- long_df$param
    facet_labeller <- ggplot2::as_labeller(units_mapping)
  }
  
  x_label <- if (aggregate=="none" && !is.null(requested_day_label)) paste0("<b>", requested_day_label, "</b>") else "<b>Time</b>"
  
  # Plot
  p <- ggplot2::ggplot(long_df, ggplot2::aes(
    x = datetime,
    y = mean_value,
    color = param,
    group = param,
    text = main_text
  )) +
    ggplot2::geom_line(size=0.8) +
    ggplot2::geom_point(size=2) +
    ggplot2::scale_color_manual(values=colors_map, labels=legend_labels) +
    ggplot2::facet_wrap(~ facet_var, ncol=1, scales="free_y", labeller=facet_labeller) +
    ggplot2::labs(x = x_label, y=NULL, color=NULL, title = ifelse(is.null(title), "<b>Time trend</b>", title)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face="bold"),
                   plot.title = ggplot2::element_text(hjust=0.5),
                   legend.position="top")
  
  if (aggregate %in% c("daily_mean", "daily_median")) {
    p <- p + ggplot2::scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d")
  }
  
  # Ribbon SD and secondary markers
  is_daily_mean <- aggregate == "daily_mean"
  
  if (is_daily_mean && any(!is.na(long_df$sd_value))) {
    ribbon_df <- long_df[!is.na(long_df$sd_value), , drop = FALSE]
    
    p <- p + ggplot2::geom_ribbon(
      data = ribbon_df,
      ggplot2::aes(
        x = datetime,
        ymin = mean_value - sd_value,
        ymax = mean_value + sd_value,
        group = param,
        fill = param 
      ),
      alpha = 0.2,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
      ggplot2::scale_fill_manual(values=colors_map)
    
    has_sd_idx <- !is.na(long_df$sd_value)
    upper_df <- long_df[has_sd_idx, , drop = FALSE]
    lower_df <- long_df[has_sd_idx, , drop = FALSE]
    
    upper_df$y <- upper_df$mean_value + upper_df$sd_value
    upper_df$text_sd <- paste0(upper_df$main_text, "<br>Upper SD limit: ", round(upper_df$y, 2))
    
    lower_df$y <- lower_df$mean_value - lower_df$sd_value
    lower_df$text_sd <- paste0(lower_df$main_text, "<br>Lower SD limit: ", round(lower_df$y, 2))
    
    p <- p +
      suppressWarnings(ggplot2::geom_point(
        data = upper_df,
        ggplot2::aes(x = datetime, y = y, text = text_sd, group = param, color = param),
        inherit.aes = FALSE,
        size = 1,
        alpha = 0.01, 
        show.legend = FALSE
      )) +
      suppressWarnings(ggplot2::geom_point(
        data = lower_df,
        ggplot2::aes(x = datetime, y = y, text = text_sd, group = param, color = param),
        inherit.aes = FALSE,
        size = 1,
        alpha = 0.01, 
        show.legend = FALSE
      ))
  }
  
  # Conversion and legend cleanup
  gp <- plotly::ggplotly(p, tooltip="text")
  
  for (i in seq_along(gp$x$data)) {
    if (!is.null(gp$x$data[[i]]$name)) {
      clean_name <- gsub(",\\d+\\)$", "", gp$x$data[[i]]$name)
      clean_name <- gsub("^\\(", "", clean_name)
      gp$x$data[[i]]$name <- clean_name
      gp$x$data[[i]]$legendgroup <- clean_name
      if (any(duplicated(sapply(gp$x$data[1:i], function(x) x$name)))) {
        gp$x$data[[i]]$showlegend <- FALSE
      }
    }
  }
  
  return(gp)
}