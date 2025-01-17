#' Get data of reflectance (level2) from WISPstation
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the data of reflectance from WISPstation.
#' @param version A `character`. It is the version of the API data. Default
#' is "1.0"
#' @param time_from A `character`. It is the time  
#' @param time_to A `character`. It is the time
#' @param station A `character`.  It is the name of the station.
#' @param userid A `character`. It is the userid.
#' @param pwd A `character`. It is the password.
#' @return A tibble with measurement id, measurement date, instrument name,
#' waterquality values of TSM, Chla and Kd as provided by instrument by default,
#' and all the values of reflectance for each wevelength from 350 to 900 nm.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @importFrom httr2 request req_url_query req_auth_basic req_perform
#' @importFrom httr2 resp_body_string
#' @importFrom tibble as_tibble
#' @importFrom dplyr slice mutate across starts_with rename_with
#' @importFrom tidyr unnest_wider
#' @importFrom lubridate as_datetime
#' @importFrom units set_units
#' @importFrom stringr str_c
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' # NA data
#' reflec_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2024-09-01T09:00",
#'   time_to = "2024-09-01T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#' 
#' # with data
#' reflec_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2024-08-01T09:00",
#'   time_to = "2024-08-01T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#'
#' # no data for the station selected
#' reflec_data <- WISP.data::wisp_get_reflectance_data(
#'   time_from = "2019-06-20T09:00",
#'   time_to = "2019-06-20T14:00",
#'   station = "WISPstation012",
#'   userid = userid,
#'   pwd = pwd
#' )
#' 
#' # The two dates are not consistent
#' reflec_data <- WISP.data::wisp_get_reflectance_data(
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
        INCLUDE = "measurement.id,measurement.date,instrument.name,waterquality.tsm,waterquality.chla,waterquality.kd,waterquality.cpc,level2.reflectance,level2.quality"
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
          INCLUDE = "measurement.id,measurement.date,instrument.name,waterquality.tsm,waterquality.chla,waterquality.kd,waterquality.cpc,level2.reflectance,level2.quality"
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
       
        dplyr::relocate(
          level2.quality, .after = instrument.name # Rearrange columns
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
    message("\n----\nPlease check the 'time_from' and 'time_to' parameters.\n\nThe two dates are not consistent.\n\nThe dates must be equal.\n----\n")
  }
  reflectance_data_tbl
}

#' Quality Check (QC) for WISPstation reflectance data
#' @description
#' This function removes all anomalous spectral signatures
#' @description `r lifecycle::badge("experimental")`
#' @param data A `tibble`. From wisp_get_reflectance_data() function.
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
#' reflec_data_qc <- WISP.data::qc_reflectance_data(data = reflec_data)
#' }
#' ## End (Not run)
#' 
### qc_reflectance_data
qc_reflectance_data <- function(data) {
 
  # Removal lines with negative values below 750 nm
  columns_nm_below_750 <- grep("^nm_([0-6][0-9]{2}|7[0-4][0-9])", colnames(data), value = TRUE)
  data[columns_nm_below_750] <- lapply(data[columns_nm_below_750], function(x) as.numeric(x))
  reflectance_data_filtered <- data |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(columns_nm_below_750), ~ . >= 0))
  
  # Removal lines with outliers in the NIR (840 nm > 700 nm)
  reflectance_data_filtered$nm_700 <- as.numeric(reflectance_data_filtered$nm_700)
  reflectance_data_filtered$nm_840 <- as.numeric(reflectance_data_filtered$nm_840)
  reflectance_data_filtered <- reflectance_data_filtered |>
    dplyr::filter(nm_840 <= nm_700)
  
  # Removal lines with maximum peak greater than 0.05 (VALUTARE)
  columns_nm <- grep("^nm_", colnames(reflectance_data_filtered), value = TRUE)
  reflectance_data_filtered[columns_nm] <- lapply(reflectance_data_filtered[columns_nm], function(x) as.numeric(x))
  reflectance_data_filtered <- reflectance_data_filtered |>
    dplyr::rowwise() |>
    dplyr::filter(max(dplyr::c_across(dplyr::all_of(columns_nm))) <= 0.05) |>
    dplyr::ungroup()
  
  # Removal lines with outliers in the Blue domain (350 nm > green; green > cyan)
  reflectance_data_filtered <- reflectance_data_filtered |>
    dplyr::filter(!(nm_350 > pmax(nm_555, nm_560, nm_565, nm_570, nm_575) & 
                      pmax(nm_555, nm_560, nm_565, nm_570, nm_575) > nm_495))
  
  # Removal of lines similar to "decreasing logarithms" (check; 350-500nm; 95%; diff<0)
  columns_nm_range <- grep("^nm_(3[5-9][0-9]|4[0-9]{2}|500)$", colnames(reflectance_data_filtered), value = TRUE)
  reflectance_data_filtered <- reflectance_data_filtered |>
    dplyr::rowwise() |>
    dplyr::filter({
      valori <- dplyr::c_across(dplyr::all_of(columns_nm_range))
      diff_valori <- diff(valori)
      percentage_negative <- mean(diff_valori < 0, na.rm = TRUE)
      percentage_negative < 0.95
    }) |>
    dplyr::ungroup()
  
  # Removal of "invalid" lines (level2.quality)
  reflectance_data_filtered <- reflectance_data_filtered |>
    dplyr::filter(level2.quality != "invalid")
  
  reflectance_data_filtered <- reflectance_data_filtered |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("nm_"), ~ units::set_units(as.numeric(as.character(.)), "1/sr")
      )
    )
  
  if (nrow(reflectance_data_filtered) == 0) {
    reflectance_data_filtered = NULL
    message("\n----\nThank you for your request, but the QC operation removed all the spectral signatures available on this date.\n----\n")
  } else {
    reflectance_data_filtered
  }
}

#' SUNGLINT Removal (SR) for WISPstation reflectance data
#' @description
#' This function applies the algorithm of Jiang et al., (2020) for removing sunglint from spectral signatures
#' @description `r lifecycle::badge("experimental")`
#' @param qc_data A `tibble` from qc_reflectance_data() function.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate select all_of across
#' @importFrom units set_units
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' reflect_data_sr <- WISP.data::sr_reflectance_data(qc_data = reflect_data_qc) 
#' }
#' ## End (Not run)
#'
### sr_reflectance_data
sr_reflectance_data <- function(qc_data) {
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
#' @description
#' This function return a plotly of each spectral signature measured by a WISPstation.
#' @description `r lifecycle::badge("experimental")`
#' @param data A `tibble` obtained by any of the functions provided by this
#' package: wisp_get_reflectance_data(), or after QC and SR removal operations.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate select
#' @importFrom plotly plot_ly layout
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ## Not run:
#' WISP.data::plot_reflectance_data(data = reflec_data_sr)
#' }
#' ## End (Not run)
#'
### plot_reflectance_data
plot_reflectance_data <- function(data) {
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

  fig <- plotly::plot_ly(
    data_2,
    x = ~wavelength,
    y = ~Rrs,
    color = ~measurement_info,
    type = 'scatter',
    mode = 'lines'
  ) |>
    plotly::layout(
      title = paste0(
        "Aquired by: ",
        data$instrument.name[1],
        "\non the date: ",
        substr(data$measurement.date[1], 1, 10)
      ),
      # plot_bgcolor = "#e5ecf6",
      xaxis = list(title = 'Wevelength [nm]'), 
      yaxis = list(title = 'Rrs [1/sr]'),
      legend = list(title = list(text = '<b>Time of aquisition</b>'))
    )
  
  fig
}
