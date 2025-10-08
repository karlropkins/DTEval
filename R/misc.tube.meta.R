##################################################
#' @title Working with Diffusion Tube Meta Data
##################################################

#' @name misc.tube.meta
#' @aliases misc.tube.meta addTubeMeta checkTubeMeta extractTubeMeta
#' padTubeMeta
#' @description Miscellaneous code used to work with
#' diffusion tube (DT) meta data.

# general meta data code
# mainly fixes at the moment...

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param ref (For \code{addTubeMeta} only) The meta data source, typically
#' a data.frame or similar, containing meta-information associated with the
#' diffusion tube records in \code{data}. See Details below.
#' @param x (For most \code{...TubeMeta} functions) The name of a data-series in
#' \code{data} or expression to be evaluated, supplied as a character string.
#' See Details below.
#' @param by The name of a data-series that can be used as a
#' case identifier for meta information. For most diffusion tube meta data,
#' this is often a site identifier, e.g. a site code or name. If \code{by}
#' is not supplied, the function will check for known \code{DTEval}
#' \code{tags} and data set details, but may still require user input. See
#' Details below.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{addTubeMeta} attempts to merge \code{data} and \code{ref} using
#' \code{by} as the common merging-term. It is intended for use with reference
#' meta data or data extracted with from a reliable source using
#' \code{extractTubeMeta}.
#'
#' \code{extractTubeMeta} attempts to extract data that looks like meta
#' information from the supplied \code{data}. Meta-data
#' is expected to unique at a specific level of aggregation, e.g. all
#' \code{latitude} records should be identical for a given sample site.
#' Although the function default is to look for known sample identifiers,
#' this grouping term can be set using the addition argument \code{by}.
#' The function then extracts all data-series with only one unique
#' non-\code{NA} value for case of \code{by}. By default, the function
#' tests all non-grouping data-series, but similarly testing/extraction
#' can be limited to specific cases using \code{x}. Please remember that
#' this function extracts data-series that at the \code{by} grouping-level
#' looks like meta information. This is no unambiguous test to identify a
#' data-series as meta-data. So, this function needs to be handled with care,
#' especially if you have any concerns about the quality of your data sets.
#'
#' \code{padTubeMeta} attempts to pad the \code{x} data-series by replacing
#' any \code{NA}s with the first non-\code{NA} entry from the same data-series.
#' It is intended for use with meta-data data-series, e.g. a \code{latitude}
#' (or \code{longitude column}) where value was only entered once. Again, care
#' should be taken using this function.
#'
#' @return These functions are generally intented to be used in the form:
#'
#' \code{updated.data <- addTubeMeta(dt.data, ref, "[by.name]")}
#'
#' \code{requested.data <- padTubeMeta(dt.data, "[meta.name]", "[by.name]")}
#'
#' ... etc
#'
#' \code{addTubeMeta} returns the supplied \code{data} with \code{ref}
#' merged at the requested level.
#'
#' \code{extractTube} returns a \code{data.frame} of data that looks like
#' meta-data when grouped at the requested level.
#'
#' \code{padTubeMeta} returns \code{data} with the requested fix,
#' if it can be applied.
#'



######################################
# to think about
######################################

#






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


#' @rdname misc.tube.meta
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





###############################
# checkTubeMeta
###############################

# might want to check through getTubeX

# notes
#############################


#' @rdname misc.tube.meta
#' @export

checkTubeMeta <- function(data, x=NULL, by=NULL, ...){

  .d <- tagTube(data)

  if(by==".location"){
    .d$.location <- paste(.d$.latitude, .d$.longitude, sep=",")
  }

  out <- calcTubeStat(.d, x, by,
                      function(x) {list(
                        count = length(unique(x, na.rm=FALSE)),
                        options = paste("'", sort(unique(x, na.rm=FALSE)), "'",
                                        sep="", collapse = "|")
                      )})
  #out <- subset(out, out[,paste(x, ".count", sep="")]!=1)
  out <- out[order(out[,paste(x, ".count", sep="")], decreasing = TRUE),]
  out
}











#################################
# extractTubeMeta
#################################

# get information that looks like meta data
# from supplied data...

# revision using calcTubeStat

# to think above
#############################

# compare this and older method in padTubeMeta
# not sure which is best..?

#' @rdname misc.tube.meta
#' @export

extractTubeMeta <- function (data, x = NULL, by = NULL, ...)
{
  if (is.null(by)) {
    if (".sample_id" %in% names(data)) {
      by <- ".sample_id"
    }
    else {
      stop("[padTubeMeta] Sorry, need a valid 'by'", call. = FALSE)
    }
  }
  if (is.null(x)) {
    x <- names(data)[!names(data) %in% by]
  }
  test <- calcTubeStat(data, x, by, function(x){length(unique(x[!is.na(x)]))})
  test <- x[apply(test[x],2, function(x) {all(x==1)})]
  if(length(test)<1){
    return(NULL)
  } else {
    calcTubeStat(data, test, by, function(x){unique(x[!is.na(x)])[1]})
  }
}





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

#' @rdname misc.tube.meta
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





