##################################################
#' @title Diffusion Tube Meta Data
##################################################

#' @name test.tube.meta
#' @aliases tube.meta testTubeMeta
#' @description Testing diffusion tube meta-data. Coded methods are all
#' currently based on the visualisation of meta-data-like behaviour.

# testTubeMeta plot

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param x (Optional) The name of one or more data-series in \code{data} or
#' expressions to be evaluated, supplied as character strings. If not supplied,
#' this defaults to all data-series in \code{code}
#' @param by (Optional) The name of one or more data-series or tags to be
#' used as a case identifier for meta information. If not supplied, this
#' defaults to the \code{all.data}, \code{sample}, \code{location} and
#' \code{date} properties of the data set tags. See Details below.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{testTubeMeta} generates a plot of meta-like behaviour. It is
#' currently in-development but is intended to be used in the form:
#'
#' \code{testTubeMeta(dt.data)}
#'
#' \code{testTubeMeta} attempts to identify data-series that look like meta
#' information in the supplied \code{data}. By its nature, meta-data
#' is expected to unique at a specific level of aggregation. For example,
#' all samples collected at one site should have the same \code{latitude},
#' \code{longitude}, sample type, etc regardless of their date of collection.
#' By default, \code{testTubeMeta} looks for data-series that are meta-like
#' at four levels of aggregation: \code{all.data} indicating that all records
#' are identical, \code{sample} indicating that all records for a given
#' \code{location} and \code{date} combination are identical; \code{location}
#' indicating that all records for a given \code{location} are identical; and
#' \code{date} indicating that all records for a given \code{date} are
#' identical. Any of these that score 100 percentage indicate meta-like
#' behaviour at the associated level of aggregation. Although, a meta-like does
#' not prove a data-series is meta-data, less than 100 percent scores for a
#' data-series that is expected to be meta-data, would indicate some issues
#' with that data-series.
#'
#' Although the function
#' default is to look for known sample identifiers, this grouping term
#' is set using the addition argument \code{by}. The function then extracts
#' all data-series with only one unique non-\code{NA} value. By default,
#' the function test all non-grouping data-series, but similarly
#' testing/extraction can be limited to specific cases using \code{x}.
#' Please remeber that this function extracts data-series that at the
#' \code{by} grouping-level looks like meta information.
#'
#' @return \code{testTubeMeta} generates a plot of meta-like behaviour.
#' It is a standard \code{ggplot2} output that can be modified using
#' standard \code{ggplot2} methods.
#'
#' @seealso [checkTubeMeta()] and related functions for working with and
#' fixing Diffusion Tube meta-data.


######################################
# to think about
######################################

# would like a getTubeMeta if possible
#     maybe call it extractTubeMeta
#     but maybe need a testAsTubeMeta as part of the setup



#############################
# testTubeMeta
#############################

# might want to check through getTubeX

# notes
#############################


#' @rdname tube.meta
#' @export

testTubeMeta <- function(data, x=NULL, by=NULL, ...){

  #setup
  .d <- tagTube(data)
  if(is.null(x)){
    x <- names(.d)
  }
  ####################################
  #by
  ####################################
  #   to do
  #   once sample/location/date sorted
  ####################################

  # add temp versions of all, sample, location, date...
  #   currently need these BUT don't keep them
  .d$..location <- paste(.d$.latitude, .d$.longitude, sep=",")
  .d$..date <- tagTubeDate(.d)$.date
  .d$..sample <- .d$.sample_id
  .d$..all <- 1

  loc.fun <- function(x){
    if(length(unique(x))==1){
      as.integer(length(x[!is.na(x)]))
    } else {
      as.integer(0)
    }
  }

  #################################################
  # count meta-like
  #################################################
  # think there will be a quicker
  #   way of doing this ???
  # currently does...
  #   how many unique length 1 w/o NAs
  # like to also know...
  #   how many unique length 1 & NA
  #   how many unique length 2 & NA + something else
  #################################################
  .test <- calcTubeStat(.d, x, "..all", loc.fun)
  .test$ref<- "all.data"
  .test <- .test[names(.test)!="..all"]
  out <- data.table::as.data.table(.test)
  .test <- calcTubeStat(.d, x, "..sample", loc.fun)
  .test$ref<- "sample"
  out <- data.table::rbindlist(list(out, .test[names(.test)!="..sample"]),
                               fill=TRUE)
  .test <- calcTubeStat(.d, x, "..date", loc.fun)
  .test$ref<- "date"
  out <- data.table::rbindlist(list(out, .test[names(.test)!="..date"]),
                               fill=TRUE)
  .test <- calcTubeStat(.d, x, "..location", loc.fun)
  .test$ref<- "location"
  out <- data.table::rbindlist(list(out, .test[names(.test)!="..location"]),
                               fill=TRUE)
  stat <- function(x) sum(x, na.rm=TRUE)
  tube <- names(out)[names(out) != "ref"]
  out <- out[, as.list(unlist(lapply(.SD, stat))), .SDcols = tube,
             by = "ref"]

  out <- as.data.frame(data.table::melt(out, id.vars="ref"))
  out$ref <- factor(out$ref, levels=rev(c("all.data", "sample",
                                          "location", "date")))

  ##############################
  # plot
  ##############################
  # flip legend list ?
  # make x-axis percentage
  ggplot2::ggplot(out) +
    ggplot2::geom_col(ggplot2::aes(x=value, y=ref, fill=ref),
                      position="dodge") +
    ggplot2::geom_vline(xintercept = nrow(.d), linetype="dashed") +
    ggplot2::facet_wrap(.~variable) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill=NA))
}






