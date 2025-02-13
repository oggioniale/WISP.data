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
#' # NA data
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
  reflectance_data_filtered <- data |> 
    dplyr::filter(dplyr::if_all(dplyr::all_of(columns_nm_below_845), ~ . >= 0))
  
  # QC2 -> Removal lines with outliers in the NIR (840 nm > 700 nm)
  reflectance_data_filtered$nm_700 <- as.numeric(reflectance_data_filtered$nm_700)
  reflectance_data_filtered$nm_840 <- as.numeric(reflectance_data_filtered$nm_840)
  removed_QC2 <- reflectance_data_filtered[reflectance_data_filtered$nm_840 > reflectance_data_filtered$nm_700, ]
  removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC2$measurement.date] <- 
    paste(removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC2$measurement.date], "QC2", sep = " ")
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::filter(nm_840 <= nm_700)
  
  # QC3 -> Removal lines with maximum peak greater than "maxPeak"
  columns_nm <- grep("^nm_", colnames(reflectance_data_filtered), value = TRUE)
  reflectance_data_filtered[columns_nm] <- lapply(reflectance_data_filtered[columns_nm], as.numeric)
  removed_QC3 <- reflectance_data_filtered[apply(reflectance_data_filtered[columns_nm], 1, max) > maxPeak, ]
  removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC3$measurement.date] <- 
    paste(removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC3$measurement.date], "QC3", sep = " ")
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::rowwise() |> 
    dplyr::filter(max(dplyr::c_across(dplyr::all_of(columns_nm))) <= maxPeak) |> 
    dplyr::ungroup()
  
  # QC4 -> Removal lines with outliers in the Blue domain
  removed_QC4 <- reflectance_data_filtered[which(
    reflectance_data_filtered$nm_350 > pmax(reflectance_data_filtered$nm_555,
                                            reflectance_data_filtered$nm_560,
                                            reflectance_data_filtered$nm_565,
                                            reflectance_data_filtered$nm_570,
                                            reflectance_data_filtered$nm_575) &
      pmax(reflectance_data_filtered$nm_555,
           reflectance_data_filtered$nm_560,
           reflectance_data_filtered$nm_565,
           reflectance_data_filtered$nm_570,
           reflectance_data_filtered$nm_575) > reflectance_data_filtered$nm_495
  ), ]
  removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC4$measurement.date] <- 
    paste(removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC4$measurement.date], "QC4", sep = " ")
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::filter(!(nm_350 > pmax(nm_555, nm_560, nm_565, nm_570, nm_575) & 
                      pmax(nm_555, nm_560, nm_565, nm_570, nm_575) > nm_495))
  
  # QC5 -> Removal of lines similar to "decreasing logarithms"
  columns_nm_range <- grep("^nm_(3[5-9][0-9]|4[0-9]{2}|500)$", colnames(reflectance_data_filtered), value = TRUE)
  removed_QC5 <- reflectance_data_filtered |> 
    dplyr::rowwise() |> 
    dplyr::filter({
      valori <- dplyr::c_across(dplyr::all_of(columns_nm_range))
      diff_valori <- diff(valori)
      percentage_negative <- mean(diff_valori < 0, na.rm = TRUE)
      percentage_negative >= 0.95
    }) |> 
    dplyr::ungroup()
  removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC5$measurement.date] <- 
    paste(removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC5$measurement.date], "QC5", sep = " ")
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::rowwise() |> 
    dplyr::filter({
      valori <- dplyr::c_across(dplyr::all_of(columns_nm_range))
      diff_valori <- diff(valori)
      percentage_negative <- mean(diff_valori < 0, na.rm = TRUE)
      percentage_negative < 0.95
    }) |> 
    dplyr::ungroup()
  
  # QC6 -> Removal of "invalid" lines (level2.quality)
  removed_QC6 <- reflectance_data_filtered[!(reflectance_data_filtered$level2.quality %in% c("okay", "suspect")), ]
  removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC6$measurement.date] <- 
    paste(trimws(removed_rows$reason[reflectance_data_filtered$measurement.date %in% removed_QC6$measurement.date]), "QC6", sep = " ")
  reflectance_data_filtered <- reflectance_data_filtered |> 
    dplyr::filter(!level2.quality %in% c("invalid", "none"))
  
  # Output message
  final_nrow <- nrow(reflectance_data_filtered)
  removed_count <- initial_nrow - final_nrow
  removed_rows <- removed_rows[removed_rows$reason != "", ]
  removal_summary <- table(trimws(removed_rows$reason))
  
  qc_descriptions <- c(
    QC1 = "remove spectral signatures with negative values below 845 nm",
    QC2 = "remove spectral signatures with outliers in the NIR (840 nm > 700 nm)",
    QC3 = "remove spectral signatures with maximum peak greater than maxPeak",
    QC4 = "remove spectral signatures with outliers in the Blue domain",
    QC5 = "remove spectral signatures similar to 'decreasing logarithms' functions",
    QC6 = "remove 'invalid' spectral signatures according to level2.quality"
  )
  
  message("\n----")
  message(removed_count, " spectral signatures were removed during QC:\n")
  
  for (qc_combo in names(removal_summary)) {
    message("- ", removal_summary[qc_combo], " spectral signatures were removed thanks to ", gsub(" ", "+", qc_combo))
  }
  
  message("")
  
  for (i in seq_len(nrow(removed_rows))) {
    message("The spectral signature of ", sub("\\..*", "", removed_rows$measurement.date[i]),
            " has been removed thanks to: ", trimws(removed_rows$reason[i]))
  }
  
  message("")
  
  used_qc <- unique(unlist(strsplit(trimws(paste(removed_rows$reason, collapse = " ")), "\\s+")))
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
#' This function applies the algorithm of Jiang et al., (2020) for removing sunglint from spectral signatures
#' @param qc_data A `tibble` from wisp_qc_reflectance_data() function.
#' @return A tibble with the spectral signatures after the SR operation.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate select all_of across
#' @importFrom units set_units
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data_sr <- WISP.data::wisp_sr_reflectance_data(qc_data = reflect_data_qc) 
#' }
#' ## End (Not run)
#'
### wisp_sr_reflectance_data
wisp_sr_reflectance_data <- function(qc_data) {
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
  
  return(corrected_data)
}

#' Create a plot of reflectance data
#' @description `r lifecycle::badge("experimental")`
#' This function return a plotly of each spectral signature measured by a
#' WISPstation.
#' @param data A `tibble` obtained by any of the functions provided by this
#' package: wisp_get_reflectance_data(), or after QC and SR removal operations.
#' @return description A plotly object with the spectral signatures of the reflectance
#' data.
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
#' WISP.data::wisp_plot_reflectance_data(data = reflect_data_sr)
#' }
#' ## End (Not run)
#'
### wisp_plot_reflectance_data
wisp_plot_reflectance_data <- function(data) {
  data_2 <- data |>
    dplyr::select(
      measurement.date, starts_with("nm_"),
      waterquality.tsm,
      waterquality.chla,
      waterquality.kd,
      waterquality.cpc
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("nm_"),
      names_to = "wavelength",
      values_to = "Rrs"
    ) |>
    # Convert wavelength name to numeric format
    dplyr::mutate(
      wavelength = as.numeric(gsub("nm_", "", wavelength)),
      measurement_info = as.factor(
        paste0(
          "Time: ", substr(measurement.date, 12, 20),
          "\nTSM [g/m3]: ", waterquality.tsm,
          "\nChla [mg/m3]: ", waterquality.chla,
          "\nKd [1/m]: ", waterquality.kd,
          "\ncpc [mg/m3]: ", waterquality.cpc
        )
      )  # Convert to factor for colours
    )

  # Color palette selection
  num_colors <- length(unique(data_2$measurement_info))
  color_palette <- viridis::viridis(num_colors)
  
  # select dates
  length_dates <- length(data$measurement.date)
  n_dates <- seq.Date(
    from = as.Date(data$measurement.date[1]),
    to = as.Date(data$measurement.date[length_dates]),
    by = "day"
  )
  if (length(n_dates) > 1) {
    dates_text <- paste0(
      "\nfrom date: ",
      n_dates[1],
      "\nto date: ",
      n_dates[length(n_dates)]
    )
  } else {
    dates_text <- paste0(
      "\non the date: ",
      n_dates[1]
    )
  }
  
  # plot
  fig <- plotly::plot_ly(
    data_2,
    x = ~wavelength,
    y = ~Rrs,
    color = ~measurement_info,
    colors = color_palette, # Assign custom color palette
    type = 'scatter',
    mode = 'lines'
  ) |>
    plotly::layout(
      title = paste0(
        "Aquired by: ",
        data$instrument.name[1],
        dates_text
      ),
      # plot_bgcolor = "#e5ecf6",
      xaxis = list(title = 'Wevelength [nm]'), 
      yaxis = list(title = 'Rrs [1/sr]'),
      legend = list(title = list(text = '<b>Time of aquisition</b>'))
    )
  
  fig
}
