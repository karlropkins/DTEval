#######################################################
#' @title Common Diffusion Tube Calculations
#######################################################

#' @name calc.tube
#' @aliases calc.tube calcTubeStat
#' @description Functions for common diffusion tube (DT)
#' data calculations using \code{DTEval}.

# common calc functions

############################################################
############################################################
# If you are doing a lot of this you should
#       learn data.table or dplyr....
############################################################
############################################################

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param tube The name of the data-series (in \code{data}) to calculate,
#' the requested statistic (or statistics) for (see \code{stat}),
#' typically the DT NO2 concentrations (in ug/m3).
#' @param by The name(s) of any grouping terms. If none are supplied,
#' total sample statistic(s) are calculated for \code{data}.
#'
#' @param stat The statistic to calculate, by default the mean in the form:
#'
#' \code{mean(x, na.rm=TRUE)}
#'
#' But it can be a list of one or more functions, ideally supplied in
#' the form:
#'
#' \code{function(x){list(mean=mean(x, na.rm), sd=sd(x, na.rm=TRUE))}}
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{calcTubeStat} attempts to calculate summary statistics.
#'
#' It and related functions assume that \code{data} is data
#' \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return a \code{data.frame} of requested
#' \code{data} statistics.



#############################
# calcTubeStat
#############################

# need to document use of data.table
#     and suggestion in stackoverflow...

# like summarise in dplyr but with data.table
#     that understand tube tagging
#         allows tube=c("measurement", "year_of_measurement")
#         allows stat=function(x){c(mean = mean(x, na.rm=TRUE), med=median(x, na.rm=TRUE))}

# functions has issue if tube includes a names with a comma in it
#   Error in `[.data.table`(d2, , as.list(unlist(lapply(.SD, stat))), .SDcols = tube,  :
#   'by' is a character vector length 3 but one or more items include a comma. Either pass
#   'a vector of column names (which can contain spaces, but no commas), or pass a vector
#   length 1 containing comma separated column names. See ?data.table for other possibilities.

# currently does
###############################
#     basic calculation
#         stat can be set as a functions
#


#  thinking about
##############################
#     output column names will get complex...
#     like to hide warnings
#        see e.g.
#            calcTubeStat(dont.share::dt.bradford.2, tube=c("measurement", "month"),
#                         stat=function(x){c(mean = mean(x, na.rm=TRUE), med=median(x, na.rm=TRUE))})
#     like to simply the stat input options
#     by arg shortcuts ???, e.g.
#         date (set date if not there and used this...)
#              (by extension year, month, etc)
#         site (unique lat/lon combinations)
#         sample (unique lat/lon/dates)...

# example????
##############################
#   calcTubeStat(dt.gla, by="local_authority")

#   #NB the space trips Site[space]Type... need to ``
#   calcTubeStat(dt.gla, by="`Site Type`")
#   #that messy???

#' @rdname calc.tube
#' @export

calcTubeStat <- function(data, tube = ".value", by = NULL, stat = NULL, ...){

  # tag data
  #   testing required tagging only
  #d2 <- data
  d2 <- tagTubeRequired(data, required=c(tube, by), ...)

  # data checks
  d2 <- checkTubeData(d2, tube, if.err="stop<<calcTubeStat>>tube")
  d2 <- checkTubeData(d2, by, if.err="stop<<calcTubeStat>>by")

  # stat handling
  if(is.null(stat)){
    stat <- function(x) { list(mean=mean(x, na.rm=TRUE)) }
  }

  # calc...
  d2 <- data.table::as.data.table(d2)
  #out <- d2[, .(stat = stat(.tube)), by=by]
  #  from https://stackoverflow.com/questions/29620783/apply-multiple-functions-to-multiple-columns-in-data-table
  out <- d2[, as.list(unlist(lapply(.SD, stat))), .SDcols = tube, by=by]

  # output
  out <- as.data.frame(out)
  return(out)
}

