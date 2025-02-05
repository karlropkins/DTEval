############################################
#' @title dt.york
############################################
#'
#' @name dt.york
#' @description Nitrogen dioxide (NO2) diffusion tube data for
#' monitoring locations about York, 2005 to 2023.
#'
#' @format A (63408x15) 'data.frame' object
#' \describe{
#'   \item{X.OS}{Site Easting}
#'   \item{Y.OS}{Site Northing}
#'   \item{SiteId}{Site Identifier}
#'   \item{SiteName}{Site Name}
#'   \item{CalendarYear}{Year tube was deployed/measurement taken}
## #'   \item{BiasCorrectedAv.ppb.}{Year average bias corrected measurement (in ppb)}
## #'   \item{BiasCorrectedAv.ug.m3.}{Year average bias corrected measurement (in ug/m3)}
#'   \item{month}{Month tube was deployed/measurement taken}
#'   \item{measurement.ppb}{Tube measurement (in ppb)}
#'   \item{SiteType}{Site classification}
#'   \item{InAQMA}{Site within the Air Quality Management Area (AQMA)?}
## #'   \item{Distance.to.Relevant.Exposure..m.}{Distance to relevant exposure source (m)}
#'   \item{Distance.to.Kerb.of.Nearest.Road..m.}{Distance to kerb of nearest road (m)}
#'   \item{measurement}{Tube measurement (in ug/m3)}
#'   \item{.longitude}{DTEval Tag}
#'   \item{.latitude}{DTEval Tag}
#'   \item{.start_date}{DTEval Tag}
#'   \item{.end_date}{DTEval Tag}
#'   \item{.site_id}{DTEval Tag}
#' }

#' @details
#' NO2 DT measurement and meta data sets were downloaded from York Open Data
#' (see Source), and processed as follows:
#'
#' Measurement data was wide-to-long converted by month and merged with the
#' meta data.
#'
#' Monthly measurements, described as "raw data (not bias-corrected) and
#' presented in ppb (parts per billion)", were converted to ug/m3 using:
#'
#' (Defra; EU method; 20oC and 1013mb)
#'
#' https://uk-air.defra.gov.uk/reports/cat06/0502160851_Conversion_Factors_Between_ppb_and.pdf
#'
#' \code{measurement.ppb * 1.9125}
#'
#' WGS84 Latitude and Longitude coordinate tags were calculated for the data
#' using grey.area::geoConvertBNG2LatLon and assuming X.OS and Y.OS are
#' British National Grid (BNG) Easting and Northing coordinates,
#' respectively.

#' @note This data set is from a public source but still being QA'ed/cleared.
#' So, contents may change. So, handle with care, and report any concerns.

#' @source City of York Council / York Open Data
#'
#' https://data.yorkopendata.org/dataset/diffusion-tubes-data
#' (downloaded 2024-12-02)
#'
"dt.york"
