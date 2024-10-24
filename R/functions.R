#' Get data of reflectance (level2) from WISP station
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the data of reflectance from WISP station.
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
  response <- httr2::request("https://wispcloud.waterinsight.nl/api/query") |> 
    httr2::req_url_query(
      SERVICE = "data",
      VERSION = version,
      REQUEST = "GetData",
      INSTRUMENT = station,
      TIME = paste(time_from, time_to, sep = ","),
      INCLUDE = "measurement.id,measurement.date,instrument.name,waterquality.tsm,waterquality.chla,waterquality.kd,level2.reflectance"
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
        INCLUDE = "measurement.id,measurement.date,instrument.name,waterquality.tsm,waterquality.chla,waterquality.kd,level2.reflectance"
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
  reflectance_data_tbl
}

#' Quality check (QC) for WISP station reflectance data
#' @description
#' A short description...
#' @description `r lifecycle::badge("experimental")`
#' @param data A `tibble`. From wisp_get_reflectance_data() function.
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @export
#' @examples
#' # example code
#' reflec_data_qc <- WISP.data::qc_reflectance_data(data = reflec_data)
#' 
### qc_reflectance_data
qc_reflectance_data <- function(data) {
  # Removal lines with negative values below 750 nm
  colonne_nm_sotto_750 <- grep("^nm_([0-6][0-9]{2}|7[0-4][0-9])", colnames(data), value = TRUE)
  data[colonne_nm_sotto_750] <- lapply(data[colonne_nm_sotto_750], function(x) as.numeric(x))
  reflec_data_filtrato <- data |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(colonne_nm_sotto_750), ~ . >= 0))
  
  # Removal lines with outliers in the NIR (840 nm > 700 nm)
  reflec_data_filtrato$nm_700 <- as.numeric(reflec_data_filtrato$nm_700)
  reflec_data_filtrato$nm_840 <- as.numeric(reflec_data_filtrato$nm_840)
  reflec_data_filtrato <- reflec_data_filtrato |>
    dplyr::filter(nm_840 <= nm_700)
  
  # Removal lines with maximum peak greater than 0.06 (VALUTARE)
  colonne_nm <- grep("^nm_", colnames(reflec_data_filtrato), value = TRUE)
  reflec_data_filtrato[colonne_nm] <- lapply(reflec_data_filtrato[colonne_nm], function(x) as.numeric(x))
  reflec_data_filtrato <- reflec_data_filtrato |>
    dplyr::rowwise() |>
    dplyr::filter(max(dplyr::c_across(dplyr::all_of(colonne_nm))) <= 0.06) |>
    dplyr::ungroup()
  
  reflec_data_filtrato <- reflec_data_filtrato |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("nm_"), ~ units::set_units(as.numeric(as.character(.)), "1/sr")
      )
    )
  
  reflec_data_filtrato
}

#' Create a plot of reflectance data
#' @description
#' This function return a plotly of each spectral signature measured by a WISP
#' station.
#' @description `r lifecycle::badge("experimental")`
#' @param data A `tibble`. From wisp_get_reflectance_data() function.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Nicola Ghirardi, phD \email{nicola.ghirardi@@cnr.it}
#' @importFrom dplyr mutate select
#' @importFrom plotly plot_ly layout
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' # example code
#' WISP.data::plot_reflectance_data(data = reflec_data_qc)
#' 
### plot_reflectance_data
plot_reflectance_data <- function(data) {
  data_2 <- data |>
    dplyr::select(
      measurement.date, starts_with("nm_"),
      waterquality.tsm,
      waterquality.chla,
      waterquality.kd
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
          "\nKd [1/m]: ", waterquality.kd
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
