############################################
#' @title dt.gla
############################################
#'
#' @name dt.gla
#' @description Nitrogen dioxide (NO2) diffusion tube data for
#' monitoring locations in London, 2021 to 2023.
#'
#' @format A (68640x18) 'data.frame' object
#' \describe{
#'   \item{X (m)}{Site Easting}
#'   \item{Y (m)}{Site Northing}
#'   \item{Site ID}{Site Identifier}
#'   \item{Site Name}{Site Name}
#'   \item{Site Type}{Site Classification}
#'   \item{Valid data capture for monitoring period %}{The data capture as
#'   a percentage for monitoring period}
#'   \item{Valid data capture  %}{Valid data capture for monitoring period}
#'   \item{Annual mean – raw data}{Annual mean, as reported}
#'   \item{Annual mean – annualised and bias adjusted}{Annual mean, after
#'   bias correction}
#'   \item{local_authority}{The local authority}
#'   \item{year}{Year tube was deployed/measurement taken}
#'   \item{month}{Month tube was deployed/measurement taken}
#'   \item{measurement}{The tube measurement reported for that year and month,
#'   (in ug/m3; __tbc'd__)}
#'   \item{.longitude}{DTEval Tag}
#'   \item{.latitude}{DTEval Tag}
#'   \item{.start_date}{DTEval Tag}
#'   \item{.end_date}{DTEval Tag}
#'   \item{.site_id}{DTEval Tag}
#' }
#' @details
#' NO2 DT measurement and meta data sets were downloaded from the
#' London Data Store (see Source), and processed as follows:
#'
#' Monthly measurements were wide-to-long to make month and measurement columns.
#'
#' WGS84 Latitude and Longitude coordinate tags were calculated for the data
#' using grey.area::geoConvertBNG2LatLon and assuming `X (m)` and `Y (m)` are
#' British National Grid (BNG) Easting and Northing coordinates,
#' respectively.

#' @note This data set is from a public source but still being QA'ed/cleared.
#' So, contents may change. So, handle with care, and report any concerns.

#' @source Greater London Authority / London Data Store
#'
#' https://data.london.gov.uk/publisher/gla-and-tfl-air-quality
#' (downloaded 2024-12-02)
#'
"dt.gla"
