############################################
#' @title dt.brd
############################################
#'
#' @name dt.brd
#' @description Bradford diffusion tube data set, 2020 to early 2026
#'
#' @format A (16737x13) 'data.frame' object
#' \describe{
#'   \item{site}{Site identifier code}
#'   \item{site_name}{Site name/description}
#'   \item{year_of_measurement}{Year the measurement was made (numeric, YYYY)}
#'   \item{month}{Month the measurement was made (character, 3-letter abbreviation)}
#'   \item{month_numeric}{Month the measurement was made (numeric, 1-12)}
#'   \item{latitude}{Latitude of site}
#'   \item{longitude}{Longitude of site}
#'   \item{aqsr_valid}{AQSR status (typically Valid/Invalid)}
#'   \item{local_authority}{Local Authority making these measurements}
#'   \item{measurement}{The unadjusted NO2 measurement}
#'   \item{bias_adjustment}{The bias adjustment factor used to adjust the measurement}
#'   \item{bias_adjusted_measurement}{The bias adjusted NO2 measurement}
#'   \item{annual.pc}{The estimated annual percentage coverage of measurements}
#' }
#' @source Bradford City Council
"dt.brd"
