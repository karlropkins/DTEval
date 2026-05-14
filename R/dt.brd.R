############################################
#' @title dt.brd
############################################
#'
#' @name dt.brd
#' @description Bradford diffusion tube data set, 2020 to 2025 subset of
#' tube measurements collected at sites classified as valid for
#' roadside sampling using the AQSR sampling scheme.
#'
#' @format A (11273x13) 'data.frame' object
#' \describe{
#'   \item{site}{Site identifier code}
#'   \item{site_name}{Site name/description}
#'   \item{year_of_measurement}{Year the measurement was made (numeric, YYYY)}
#'   \item{month}{Month the measurement was made (character, 3-letter abbreviation)}
#'   \item{month_numeric}{Month the measurement was made (numeric, 1-12)}
#'   \item{latitude}{Latitude of site}
#'   \item{longitude}{Longitude of site}
#'   \item{local_authority}{Local Authority making these measurements}
#'   \item{measurement}{The unadjusted NO2 measurement}
#'   \item{bias_adjustment}{The bias adjustment factor used to adjust the measurement}
#'   \item{bias_adjusted_measurement}{The bias adjusted NO2 measurement}
#'   \item{.start_date}{Sampling start date (Date, YYYY-MM-DD)}
#'   \item{.end_date}{Sampling end date (Date, YYYY-MM-DD)}
#' }
#' @source Bradford City Council
"dt.brd"
