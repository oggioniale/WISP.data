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
    
    if (df$measurement.id[2] == "-1") {
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
    message("\n----\nPlease check the 'time_from' and 'time_to' parameters.\n\nThe two dates are not consistent.\n\nThe dates must be equal. \n\nTo use multiple dates use function: wisp_get_reflectance_multi_data()\n----\n")
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
        "\n----\nSkipping %s: %s\n----\n",
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
#' We recommend setting this parameter to: 0.02 (default)
#' @param qa_threshold A `decimal`. Minimum threshold for Quality Assurance (QA).
#' We recommend setting this parameter to: 0.5 (default)
#' @param qwip_threshold A `decimal`. Maximum threshold for Quality Water Index Polynomial (QWIP).
#' We recommend setting this parameter to: 0.2 (default)
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
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#'   QA_ref_csv = "path/QA_raw_data_nRrs_Wei2016.csv"
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
    stop("No band between 350 and 450 nm available for QC4.")
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
    message("Thank you for your request, but the QC operation removed all the spectral signatures available on this date.")
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
#' This function applies the algorithm of Jiang et al., (2020) for removing
#' sunglint from spectral signatures
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
#'   calc_mishra = TRUE,
#'   save_csv = FALSE,
#'   out_dir = "outputs"
#' )
#' }
#' ## End (Not run)
#'
### wisp_sr_reflectance_data
wisp_sr_reflectance_data <- function(
    qc_data,
    calc_scatt   = TRUE,
    calc_SPM     = TRUE,
    calc_TUR     = TRUE,
    calc_TSS     = TRUE,
    calc_gons    = TRUE,
    calc_gons740 = TRUE,
    calc_NDCI    = TRUE,
    calc_mishra  = TRUE,
    save_csv     = FALSE,
    out_dir      = "outputs"
) {
  if (!"QC" %in% names(qc_data)) {
    message("\n----\nThis function is not executable on this dataset. Try after QC.\n----\n")
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
  # check and eventually remove scattering.peak and band.ratio
  corrected_data <- corrected_data |>
    dplyr::select(-dplyr::one_of("scattering.peak", "band.ratio"))
  # check and eventually remove Novoa.SPM and Blended.SPM
  corrected_data <- corrected_data |>
    dplyr::select(-dplyr::one_of("Novoa.SPM", "Blended.SPM"))
  # check and eventually remove Novoa.TUR and Blended.TUR
  corrected_data <- corrected_data |>
    dplyr::select(-dplyr::one_of("Novoa.TUR", "Blended.TUR"))
  
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
    warning("wisp_calc_Gons_CHL: missing required bands (664/704/782 ±3 nm).")
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
    warning("wisp_calc_Gons740_CHL: missing required bands (664/704/740 ±3 nm).")
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
    warning("wisp_calc_NDCI: missing required bands (670/705 ±3 nm).")
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
    stop("Error: the ‘NDCI’ column is not present. First calculate NDCI using 'wisp_calc_NDCI' function.")
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
#'   legend_mishra_CHL = FALSE
#' )
#' }
#' ## End (Not run)
#'
### wisp_plot_reflectance_data
wisp_plot_reflectance_data <- function(
    data,
    legend_TSM         = TRUE,
    legend_Chla        = TRUE,
    legend_Kd          = TRUE,
    legend_cpc         = TRUE,
    legend_scatt       = FALSE,
    legend_ratio       = FALSE,
    legend_novoa_SPM   = FALSE,
    legend_novoa_TUR   = FALSE,
    legend_jiang_TSS   = FALSE,
    legend_gons_CHL    = FALSE,
    legend_gons740_CHL = FALSE,
    legend_NDCI        = FALSE,
    legend_mishra_CHL  = FALSE
) {
  
  # Production of data information
  data <- data |>
    dplyr::mutate(
      products_info = mapply(function(tsm, chla, kd, cpc, scatt, ratio, novoa_spm, novoa_tur, jiang_tss, gons_chl, gons740_chl, ndci, mishra_chl) {
        paste(
          c(
            if (legend_TSM && !is.null(tsm)) paste("TSM [g/m3]:", tsm),
            if (legend_Chla && !is.null(chla)) paste("Chla [mg/m3]:", chla),
            if (legend_Kd && !is.null(kd)) paste("Kd [1/m]:", kd),
            if (legend_cpc && !is.null(cpc)) paste("Cpc [mg/m3]:", cpc),
            if (legend_scatt && !is.null(scatt)) paste("Scatt [1/sr]:", scatt),
            if (legend_ratio && !is.null(ratio)) paste("Ratio:", ratio),
            if (legend_novoa_SPM && !is.null(novoa_spm)) paste("Novoa_SPM [g/m3]:", novoa_spm),
            if (legend_novoa_TUR && !is.null(novoa_tur)) paste("Novoa_TUR [NTU]:", novoa_tur),
            if (legend_jiang_TSS && !is.null(jiang_tss)) paste("Jiang_TSS [g/m3]:", jiang_tss),
            if (legend_gons_CHL && !is.null(gons_chl)) paste("Gons.CHL [mg/m3]:", gons_chl),
            if (legend_gons740_CHL && !is.null(gons740_chl)) paste("Gons740.CHL [mg/m3]:", gons740_chl),
            if (legend_NDCI && !is.null(ndci)) paste("NDCI:", ndci),
            if (legend_mishra_CHL && !is.null(mishra_chl)) paste("Mishra.CHL [mg/m3]:", mishra_chl)
          ),
          collapse = "<br>"
        )
      },
      tsm         = if ("waterquality.tsm"     %in% names(data)) data$waterquality.tsm else rep(NA, nrow(data)),
      chla        = if ("waterquality.chla"    %in% names(data)) data$waterquality.chla else rep(NA, nrow(data)),
      kd          = if ("waterquality.kd"      %in% names(data)) data$waterquality.kd else rep(NA, nrow(data)),
      cpc         = if ("waterquality.cpc"     %in% names(data)) data$waterquality.cpc else rep(NA, nrow(data)),
      scatt       = if ("scattering.peak"      %in% names(data)) data$scattering.peak else rep(NA, nrow(data)),
      ratio       = if ("band.ratio"           %in% names(data)) data$band.ratio else rep(NA, nrow(data)),
      novoa_spm   = if ("Novoa.SPM"            %in% names(data)) data$Novoa.SPM else rep(NA, nrow(data)),
      novoa_tur   = if ("Novoa.TUR"            %in% names(data)) data$Novoa.TUR else rep(NA, nrow(data)),
      jiang_tss   = if ("Jiang.TSS"            %in% names(data)) data$Jiang.TSS else rep(NA, nrow(data)),
      gons_chl    = if ("Gons.CHL"             %in% names(data)) data$Gons.CHL else rep(NA, nrow(data)),
      gons740_chl = if ("Gons740.CHL"          %in% names(data)) data$Gons740.CHL else rep(NA, nrow(data)),
      ndci        = if ("NDCI"                 %in% names(data)) data$NDCI else rep(NA, nrow(data)),
      mishra_chl  = if ("Mishra.CHL"           %in% names(data)) data$Mishra.CHL else rep(NA, nrow(data))
      )
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
      title = paste0("Acquired by: ", data$instrument.name[1], dates_text),
      xaxis = list(title = '<b>Wavelength [nm]<b>', dtick = 100),
      yaxis = list(title = '<b>Rrs [1/sr]<b>'),
      legend = list(title = list(text = '<b>Time of acquisition</b>'))
    )
  
  fig
}

#' Creates a plot of the trend of one or more water quality parameters
#' @description `r lifecycle::badge("experimental")`
#' This function creates an interactive graph of the time trend of one or
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
#' Options are `"none"` (plot all available values), `"daily_mean"` (plot the daily mean) 
#' or `"daily_median"` (plot the daily median). Default is `"none"`.
#' @param smooth A `logical`. If `TRUE`, adds loess smoothing lines to the plot. Default is `FALSE`.
#' @param na.rm A `logical`. If `TRUE`, NA values are ignored during aggregation. Default is `TRUE`.
#' @param colors A `character` vector of colors for each parameter. Default uses `viridis` palette.
#' @param title A `character`. Optional title for the plot. Default is `NULL`.
#' @return An interactive `plotly` object showing the time trend of the selected parameters,
#' with optional ribbons for standard deviation.
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, PhD \email{nicola.ghirardi@@cnr.it}
#' @importFrom plotly plot_ly add_lines add_ribbons subplot layout
#' @importFrom dplyr group_by summarise
#' @importFrom lubridate as_datetime
#' @importFrom viridis viridis
#' @importFrom scales alpha
#' @export
#' @examples
#' # Example usage
#' \dontrun{
#' fig_trend <- wisp_trend_plot(
#'   data = reflect_data_sr,
#'   params = c("TSM", "Chla", "Novoa_SPM"),
#'   aggregate = "daily_mean",
#' )
#' fig_trend
#' }
wisp_trend_plot <- function(
    data,
    params = c("TSM", "Chla"),
    datetime_col = "measurement.date",
    instrument_col = "instrument.name",
    aggregate = c("none", "daily_mean", "daily_median"),
    na.rm = TRUE,
    colors = NULL,
    title = NULL
) {
  aggregate <- match.arg(aggregate)
  
  if (is.null(data) || nrow(data) == 0) stop("Il dataset 'data' è vuoto o NULL.")
  if (!datetime_col %in% names(data)) stop("Colonna datetime non trovata: ", datetime_col)
  
  mapping <- list(
    TSM           = "waterquality.tsm",
    Chla          = "waterquality.chla",
    Kd            = "waterquality.kd",
    cpc           = "waterquality.cpc",
    scatt         = "scattering.peak",
    ratio         = "band.ratio",
    Novoa_SPM     = "Novoa.SPM",
    Novoa_TUR     = "Novoa.TUR",
    Jiang_TSS     = "Jiang.TSS",
    Gons_CHL      = "Gons.CHL",
    Gons740_CHL   = "Gons740.CHL",
    NDCI          = "NDCI",
    Mishra_CHL    = "Mishra.CHL"
  )
  
  units_mapping <- c(
    TSM           = "TSM [g/m3]",
    Chla          = "Chla [mg/m3]",
    Kd            = "Kd [1/m]",
    cpc           = "cpc [mg/m3]",
    scatt         = "scatt [1/sr]",
    ratio         = "ratio",
    Novoa_SPM     = "Novoa_SPM [g/m3]",
    Novoa_TUR     = "Novoa_TUR [NTU]",
    Jiang_TSS     = "Jiang_TSS [g/m3]",
    Gons_CHL      = "Gons_CHL [mg/m3]",
    Gons740_CHL   = "Gons740_CHL [mg/m3]",
    NDCI          = "NDCI",
    Mishra_CHL    = "Mishra_CHL [mg/m3]"
  )
  
  requested <- unique(params)
  valid <- requested[requested %in% names(mapping)]
  if (length(valid) == 0) stop("Nessun parametro valido richiesto.")
  
  cols <- unlist(mapping[valid])
  present <- cols %in% names(data)
  if (!all(present)) warning("Alcuni parametri non presenti nel dataset: ",
                             paste(valid[!present], collapse = ", "))
  
  cols <- cols[present]
  valid <- valid[present]
  
  drop_units_safe <- function(x) {
    if (inherits(x, "units")) as.numeric(units::drop_units(x)) else as.numeric(x)
  }
  
  datetime <- lubridate::as_datetime(data[[datetime_col]])
  instrument <- if (instrument_col %in% names(data)) as.character(data[[instrument_col]]) else rep(NA_character_, nrow(data))
  
  long_df <- data.frame(stringsAsFactors = FALSE)
  for (i in seq_along(valid)) {
    param <- valid[i]
    col <- cols[i]
    values <- drop_units_safe(data[[col]])
    tmp <- data.frame(
      datetime = datetime,
      instrument = instrument,
      param = param,
      value = values,
      stringsAsFactors = FALSE
    )
    long_df <- rbind(long_df, tmp)
  }
  
  long_df <- long_df[!is.na(long_df$value), ]
  if (nrow(long_df) == 0) stop("Nessun valore valido dopo filtraggio NA.")
  
  # Aggregazione
  if (aggregate != "none") {
    long_df$date <- as.Date(long_df$datetime)
    grouped <- dplyr::group_by(long_df, param, date)
    long_df <- dplyr::summarise(
      grouped,
      mean_value = mean(value, na.rm = na.rm),
      sd_value   = sd(value, na.rm = na.rm),
      .groups = "drop"
    )
    long_df$datetime <- as.POSIXct(long_df$date)
  } else {
    long_df$mean_value <- long_df$value
    long_df$sd_value <- NA_real_
  }
  
  params_unique <- unique(long_df$param)
  if (is.null(colors)) colors <- viridis::viridis(length(params_unique))
  
  plots <- list()
  for (i in seq_along(params_unique)) {
    p <- params_unique[i]
    sub <- long_df[long_df$param == p, , drop = FALSE]
    param_label <- units_mapping[p]
    
    # Tooltip linea centrale
    if (aggregate == "none") {
      hover_text <- paste0(
        "Date: ", format(sub$datetime, "%Y-%m-%d"), "<br>",
        "Time: ", format(sub$datetime, "%H:%M:%S"), "<br>",
        param_label, ": ", round(sub$mean_value, 2),
        ifelse(!all(is.na(sub$sd_value)), paste0(" ± ", round(sub$sd_value, 2)), "")
      )
    } else {
      hover_text <- paste0(
        "Date: ", format(sub$datetime, "%Y-%m-%d"), "<br>",
        param_label, ": ", round(sub$mean_value, 2),
        ifelse(!all(is.na(sub$sd_value)), paste0(" ± ", round(sub$sd_value, 2)), "")
      )
    }
    
    fig <- plotly::plot_ly(
      data = sub,
      x = ~datetime,
      y = ~mean_value,
      type = "scatter",
      mode = "lines+markers",
      name = param_label,
      text = hover_text,
      hoverinfo = "text",
      line = list(width = 2),
      marker = list(size = 6),
      color = I(colors[i])
    )
    
    # Aggiungi ribbon e punti upper/lower solo se sd_value non è tutto NA
    if (!all(is.na(sub$sd_value))) {
      fig <- plotly::add_ribbons(
        fig,
        ymin = ~mean_value - sd_value,
        ymax = ~mean_value + sd_value,
        fillcolor = scales::alpha(colors[i], 0.15),
        line = list(color = 'transparent'),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
      
      fig <- plotly::add_trace(
        fig,
        x = ~datetime,
        y = ~mean_value + sd_value,
        type = "scatter",
        mode = "markers",
        marker = list(size = 6, color = 'transparent'),
        showlegend = FALSE,
        hovertemplate = paste0(param_label, " upper: %{y:.2f}<extra></extra>")
      )
      
      fig <- plotly::add_trace(
        fig,
        x = ~datetime,
        y = ~mean_value - sd_value,
        type = "scatter",
        mode = "markers",
        marker = list(size = 6, color = 'transparent'),
        showlegend = FALSE,
        hovertemplate = paste0(param_label, " lower: %{y:.2f}<extra></extra>")
      )
    }
    
    plots[[i]] <- plotly::layout(fig, yaxis = list(title = paste0("<b>", param_label, "</b>")))
  }
  
  final_fig <- plotly::subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    titleY = TRUE
  )
  
  final_fig <- plotly::layout(
    final_fig,
    title = ifelse(is.null(title),
                   paste("Time trend:", paste(units_mapping[params_unique], collapse = ", ")),
                   title),
    xaxis = list(title = "<b>Time</b>")
  )
  
  return(final_fig)
}













