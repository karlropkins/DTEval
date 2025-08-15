##################################################
#' @title Miscellaneous Diffusion Tube Meta Data
##################################################

#' @name misc.dt.meta
#' @aliases misc.dt.meta padTubeMeta addTubeMeta
#' @description Miscellaneous code used to work with
#' diffusion tube (DT) meta data.

# general meta data code
# mainly fixes at the moment...

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param ref (For \code{addTubeMeta} only) The meta data source, typically
#' a data.frame or similar, containing meta-information associated with the
#' diffusion tube records in \code{data}. See Details below.
#' @param x (For \code{padTubeMeta} only) The name of a data-series in
#' \code{data} or expression to be evaluated, supplied as a character string.
#' See Details below.
#' @param by The name of a data-series that can be used as a
#' case identifier for meta information. For most diffusion tube meta data,
#' this is often a site identifier, e.g. a site code or name. If \code{by}
#' is not supplied, the function will check for known \code{DTEval}
#' \code{tags} and file details, but may still require user input. See
#' Details below.
#' @param ... additional arguments, currently ignored.

#' @details
#' #' \code{addTubeMeta} attempts to merge \code{data} and \code{ref} using
#' \code{by} as the common term. It is intended for use then

#' \code{padTubeMeta} attempts to pad the \code{x} data-series by replacing
#' any \code{NA}s with the non-\code{NA} entries from the same data-series.
#' It is intended for use with meta-data data-series, e.g. a \code{latitude}
#' (or \code{longitude column}) where value was only entered once. Meta-data
#' is only be expected to unique at a specific level of aggregation, e.g.
#' \code{latitude} will be unique for a sample site. So, an additional
#' identifier is required to group the data, \code{by}.
#'
#' The function returns the supplied data with the attempted fix applied,
#' and is generally used in the form:
#'
#' \code{dt.data.new <- padTubeMeta(dt.data, "[meta.name]", "[by.name]")}
#'
#' @return \code{padTubeMeta} returns \code{data} with the requested fix,
#' if it can be applied.


######################################
# to think about
######################################

# would like a getTubeMeta if possible
#     maybe call it extractTubeMeta
#     but maybe need a testAsTubeMeta as part of the setup





#############################
# addTubeMeta
#############################

# might want to check through getTubeX

# notes
#############################

# replacing dplyr code with data.table
#    because we are getting summarise warning
#    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
#    but be a big job... because I'll need to go through the package and do the lot...


#' @rdname misc.dt.meta
#' @export

addTubeMeta <- function(data, ref=NULL, by=NULL,...){
  if(is.null(ref)){
    return(data)
  }
  if(is.null(by)){
    # use tag if there
    if(".sample_id" %in% names(data)){
      by <- ".sample_id"
    } else {
      stop("[addTubeMeta] Sorry, need a valid 'by'",
           call.=FALSE)
    }
  }
  #remove any common names in both
  data <- as.data.frame(data)
  test <- names(data)[names(data) %in% names(ref)]
  test <- test[!test %in% by]
  data <- data[!names(data) %in% test]
  out <- data.table::merge.data.table(data.table::as.data.table(data),
                                      data.table::as.data.table(ref), by=by,
                                      allow.cartesian = TRUE)
  out <- as.data.frame(out)
  return(out)
}

# dt.bradford <- dont.share::dt.bradford
# my.ref <- dt.bradford[c("site", "site_name", "latitude", "longitude",
#                         "aqsr_valid", "local_authority")]
# tidy <- function(x) {na.omit(unique(x))[1]}
# my.ref <- as.data.table(my.ref)[, .(
#                                     site_name = tidy(site_name),
#                                     latitude = tidy(latitude),
#                                     longitude = tidy(longitude),
#                                     aqsr_valid = tidy(aqsr_valid),
#                                     local_authority = tidy(local_authority)
#                                     ), by=c("site")]
# addTubeMeta(dt.bradford, my.ref, "site")







#############################
# padTubeMeta
#############################

# might want to check through getTubeX

# notes
#############################

# replacing dplyr code with data.table
#    because we are getting summarise warning
#    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
#    but be a big job... because I'll need to go through the package and do the lot...

# data.table switch related
#    need to tidy the data class switching

# do we want to allow other string removal (NA, "", " ", "NA", etc)???
#    maybe like [na.strings = "NA"] in read.table ???

# do we want to handle non-unique meta more robustly???
#    see in-code notes
#

#' @rdname misc.dt.meta
#' @export

padTubeMeta <- function(data, x=NULL, by=NULL,...){
  if(is.null(x)){
    return(data)
  }
  if(is.null(by)){
    # use tag if there
    if(".sample_id" %in% names(data)){
      by <- ".sample_id"
    } else {
      stop("[padTubeMeta] Sorry, need a valid 'by'",
           call.=FALSE)
    }
  }
  #################################################
  # below
  # see if.err guidance in package to tidy
  .data <- checkTubeData(as.data.frame(data), c(x, by), if.err="stop")[, c(x,by)]
  .data <- data.table::as.data.table(.data)
  ##########################
  # below
  # should it warn if length(na.omit(unique(get(x)))) >  1 ...?
  #      maybe options??
  .data <- .data[, .(out = na.omit(unique(get(x)))[1]), by=c(by)]
  data.table::setnames(.data, c("out"), x)
  data <- as.data.frame(data)
  data <- data[!names(data) %in% x]
  .data <- data.table::merge.data.table(data.table::as.data.table(data),
                                        data.table::as.data.table(.data),
                                        by=by)
  .data <- as.data.frame(.data)
  return(.data)
}

# dplyr to data.table
# https://atrebas.github.io/post/2019-03-03-datatable-dplyr/

# dt.bradford <- dont.share::dt.bradford
# dat2 <- padTubeMeta(dt.bradford, "latitude", "site_name")
# dat2 <- padTubeMeta(dat2, "longitude", "site_name")

# compare
# testTubePrecision(dt.bradford)
# testTubePrecision(dat2)

