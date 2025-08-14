#######################################################
#' @title Miscellaneous Diffusion Tube Data Statistics
#######################################################

#' @name misc.dt.stats
#' @aliases misc.dt.stats tubeSummary tubeSummarySample
#' @description Miscellaneous code to calculate some common diffusion tube
#' (DT) outputs used by \code{DTEval}.

# common summaries, stats, reports etc...

############################################################
############################################################
# think tubeSummarySample should be on a misc.dt.sample page
# think we should have a calcTubeStat type function ???
############################################################
############################################################

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{tubeSummary} attempts to generate an overview summary for the
#' supplied \code{DTEval} diffusion tube data set \code{data}.
#'
#' \code{tubeSummarySample} attempts to generate a summary of
#' sample-related counts and check sums for \code{data}.
#'
#' They are also intended to be used in the form:
#'
#' \code{tubeSummary(data)}
#'
#' And all assumes that \code{data} is data \code{DTEval} will
#' recognise as Diffusion Tube data, so either previously tagged
#' tube data or data that is tag-able using a default call of
#' \code{\link{tagTube}}

#' @return All functions return a \code{data.frame} of requested
#' \code{data} statistics.
#'


#############################
# tubeSummary
#############################

# standard DT data set summary

# currently does
###############################
#    sampling.from, sampling.to
#    n.total, n.samples, n.sites, n.suspect

#    notes
#         using tubeSummarySample to calculate n.suspect
#             this check lat/long, start/end (obviously not sample_id)

#  thinking about
##############################
#      n.intervals
#            unique .start_date + .end_date combinations
#      option for different outputs?
#             short and long versions, etc?
#      use as summary if we go to a DTEval object class
#             maybe check AQEval to see if we did there ???

#' @rdname misc.dt.stats
#' @export

tubeSummary <- function(data, ...){

  #d2 should have all tags if data is recognisable dt data
  d2 <- tagTube(data)

  #general stats
  n.total <- nrow(d2)

  #time range
  #   SHOULD check tagTubeDate checks this is a date format??
  sampling.from <- min(d2$.start_date, na.rm=TRUE)
  sampling.to <- max(d2$.end_date, na.rm=TRUE)

  #n.intervals
  #     this is the number of unique start/end dates
  test <- paste(d2$.start_date, d2$.end_date)
  n.intervals <- length(unique(test))

  #n.sites
  #     this is the number of unique lat/longs
  test <- paste(d2$.latitude, d2$.longitude)
  n.sites <- length(unique(test))

  #n.samples
  n.samples <- length(unique(d2$.sample_id))

  #n.suspect
  n.bad.tags <- nrow(subset(tubeSummarySample(d2), checksum>0))

  out <- data.frame(sampling.from, sampling.to,
                    n.total, n.samples, n.intervals, n.sites, n.bad.tags)

  return(out)

}


#############################
# tubeSummarySample
#############################

# standard DT by sample check

# currently does
###############################

#  thinking about
##############################

# not sure this is staying

# think about this

# good data
#> data.frame(t(summary(factor(tubeSummarySample(tagTube(dont.share::dt.bradford.2))$n))))
#X1  X2   X3
#1 4107 410 3111

# bad data
# (this had incomplete lat and lon entries...)
#> data.frame(t(summary(factor(tubeSummarySample(tagTube(dont.share::dt.bradford))$n))))
#X1 X2 X3 X8 X275 X284 X286 X290 X296 X297 X300 X301 X302 X305 X310 X311 X312 X314 X318 X319 X321 X329 X330
#1 2349 33 33  1    1    1    1    1    2    1    1    1    1    1    1    1    1    1    2    2    1    1    1
#X332 X335
#1    1    1


#' @rdname misc.dt.stats
#' @export

tubeSummarySample <- function(data, ...){

  #d2 should have all tags if data is recognisable dt data
  d2 <- tagTube(data)

  #making summary report with data.table
  d2 <- data.table::as.data.table(d2)
  test <- d2[,.(n = length(.latitude),
                missing.latitudes = length(.latitude[is.na(.latitude)]),
                missing.longitudes = length(.longitude[is.na(.longitude)]),
                missing.start.dates = length(.start_date[is.na(.start_date)]),
                missing.end.dates = length(.end_date[is.na(.end_date)])
              ), by = ".sample_id"]
  out <- as.data.frame(test)
  out$checksum <- out$missing.latitudes + out$missing.longitudes +
                  out$missing.start.dates + out$missing.end.dates
  return(out)

}




