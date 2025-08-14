#########################################################
#' @title Miscellaneous Date Handlers
#########################################################

# setTubeDate

#' @name misc.dt.date
#' @aliases misc.dt.date setTubeDate
#' @description Miscellaneous functions for use with diffusion tube date
#' information in \code{DTEval}.
#'
#' Diffusion tubes are typically deployed for
#' several weeks, so each sample record has an associated sampling start and
#' end date (\code{.start_date} and \code{.end_date} once tagged, see
#' \code{\link{tagTube}}) and an associated sampling interval. These functions
#' are intended as shortcuts for those unfamiliar with the \code{R} \code{Date}
#' object class used by \code{DTEval}.

# common date handlers

#######################################################
#######################################################
# think there should be a tubeSummaryDates on this page???
#######################################################
#######################################################


#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param method The method to use (see details).
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{setTubeDate} attempts to assign a representative date to each
#' sampling record (\code{data} row). By default, it set the date to the
#' mid-point between sample start and end dates, and is, for example,
#' intended for use when plotting time-series: options: 1 (start date),
#' 2 (default; mid-point date); 3 (end date).
#'
#' All functions assume \code{data} is either  previously tagged data or data
#' that can be tagged using a default call of \code{\link{tagTube}}.

#  ....

#' @return By default \code{setTubeDate} returns \code{data} with an
#' additional column, \code{.date} of sampling dates determined using
#' requested method.




#############################
# setTubeDate
#############################

# date for plot of measurement versus date
#   using mid-range rather than start or end date

# currently doing
###############################
#

# think about
##############################
#  handling for different outputs??
#       vector name
#       vector elements/type
#  overwrite/force handling???


#' @rdname misc.dt.date
#' @export

setTubeDate <- function(data, method=2, ...){

  data <- tagTube(data)
  check <- 1:3
  if(!method %in% check){
    stop("[setTubeDate] known methods: ", paste(check, collapse=","),
         call.=FALSE)
  }
  if(method==1){
    temp <- data$.start_date
  }
  if(method==2){
    temp <- data$.start_date + ((data$.end_date - data$.start_date)/2)
  }
  if(method==3){
    temp <- data$.end_date
  }
  data$.date <- temp

  return(data)

}


