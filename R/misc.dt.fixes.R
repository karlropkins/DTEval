##################################################
#' @title Miscellaneous Diffusion Tube Data Fixes
##################################################

#' @name misc.dt.fixes
#' @aliases misc.dt.fixes padTubeMeta
#' @description Miscellaneous code for fixing commonly reported issues with
#' diffusion tube (DT) data.

# general data fix code

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param x The name of a data-series in \code{data} or expression to
#' be evaluated, supplied as a character string. See Details below.
#' @param site.id The name of a data-series that can be used as a
#' sampling site identifier. If \code{site.id} is not supplied, the
#' function will check for known \code{DTEval} \code{tags}.
#' See Details below.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{padTubeMeta} attempts to pad the \code{x} data-series by replacing
#' any \code{NA}s with the non-\code{NA} entries from the same data-series.
#' It is intended for use with meta-data data-series, e.g. a \code{latitude}
#' (or \code{longitude column}) where value was only entered once. Meta-data
#' is only be expected to unique at a specific level of aggregation, e.g.
#' \code{latitude} will be unique for a sample site. So, an additional
#' identifier is required to group the data, \code{site.id}.
#'
#' The function returns the supplied data with the attempted fix applied,
#' and is generally used in the form:
#'
#' \code{dt.data.new <- padTubeMeta(dt.data, "[meta.name]", "[id.name]")}
#'
#' @return \code{padTubeMeta} returns \code{data} with the requested fix,
#' if it can be applied.


#############################
# padTubeMeta
#############################

# might want to check through getTubeX

# notes
#############################

# replacing dplyr code with data.table
# because we are getting summarise warning
# Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
# but be a big job... because I'll need to go through the package and do the lot...

# do we want to allow other string removal (NA, "", " ", "NA", etc)???

# do we want to handle non-unique meta more robustly???
# see in-code notes

#' @rdname padTubeMeta
#' @export

padTubeMeta <- function(data, x=NULL, site.id=NULL,...){
  if(is.null(x)){
    return(data)
  }
  if(is.null(site.id)){
    # use tag if there
    if(".site_id" %in% names(data)){
      site.id <- ".site_id"
    } else {
      stop("[padTubeMeta] Sorry, need a valid site.id",
           call.=FALSE)
    }
  }
  #################################################
  # below
  # see if.err guidance in package to tidy
  .data <- checkTubeData(data, c(x, site.id), if.err="stop")[, c(x,site.id)]
  .data <- data.table::as.data.table(.data)
  ##########################
  # below
  # should it warn if
  #      length(na.omit(unique(get(x)))) >  1 ...?
  .data <- .data[, .(out = na.omit(unique(get(x)))), by=c(site.id)]
  data.table::setnames(.data, c("out"), x)
  data <- data[!names(data) %in% x]
  .data <- data.table::merge.data.table(data, .data, by=site.id)
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

