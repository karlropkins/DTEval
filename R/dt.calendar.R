############################################
#' @title dt.calendar
############################################
#'
#' @name dt.calendar
#' @description The diffusion tube sampling schedule calendar
#' 2015 and 2025.
#'
#' @format A (132x5) 'data.frame' object
#' \describe{
#'   \item{year}{sampling year}
#'   \item{month}{sampling month}
#'   \item{start}{start date for sampling that year and month}
#'   \item{end}{end date for sampling that year and month}
#'   \item{weeks}{number of weeks sampled}
#' }
#' @source These were extracted from yearly sampling schedules at (or from):
#'
#' https://laqm.defra.gov.uk/diffusion-tubes/data-entry (2015-2020),
#'
#' https://laqm.defra.gov.uk/air-quality/air-quality-assessment/diffusion-tube-monitoring-calendar/ (2021),
#'
#' LAQM Helpdesk Team (2022),
#'
#' https://laqm.defra.gov.uk/air-quality/air-quality-assessment/diffusion-tube-monitoring-calendar/ (2023-2025).

#sources
#(accessed 09-2021)
#https://laqm.defra.gov.uk/documents/Timetable-2015.pdf
#https://laqm.defra.gov.uk/documents/DT-Timetable-2016-v2.pdf
#https://laqm.defra.gov.uk/documents/DT-Timetable-2017-v1.pdf
#https://laqm.defra.gov.uk/assets/dttimetable2018v1.pdf
#https://laqm.defra.gov.uk/assets/dttimetable2019v1.pdf
#https://laqm.defra.gov.uk/assets/2020laqmcalendar1.pdf
#(accessed 10-2021)
#https://laqm.defra.gov.uk/assets/2021laqmdtcalendarv1.pdf
#(email request 11-2022) [reply from Jai Mistry, LAQM Helpdesk Team; LAQMHelpdesk_[AT]_bureauveritas.com]
#(accessed 11-2023; 2023/2024)
#https://laqm.defra.gov.uk/air-quality/air-quality-assessment/diffusion-tube-monitoring-calendar/
#(accessed 11-2024; 2025)
#https://laqm.defra.gov.uk/air-quality/air-quality-assessment/diffusion-tube-monitoring-calendar/


###################################################
# to do
###################################################
# describe this data
# caveat that we are not sure if they are following
# example showing how they are handled
# think about long term upkeep of this data set

"dt.calendar"
