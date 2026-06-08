##################################################
#' @title Working with Diffusion Tube Meta Data
##################################################

#' @name misc.tube.meta
#' @aliases misc.tube.meta addTubeMeta checkTubeMeta extractTubeMeta extractAndAddTubeMeta padTubeMeta repairTubeMeta
#' @description Miscellaneous code used to work with
#' diffusion tube (DT) meta data.

# general meta data code
# mainly fixes at the moment...

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param ref (For \code{addTubeMeta} and \code{extractAndAddTubeMeta} only)
#' The meta data source, typically a data.frame or similar containing
#' meta-information associated with the diffusion tube records in \code{data}.
#' See Details below.
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
#' @param options (for \code{repairTubeMeta} only) a vector of valid options
#' for \code{x}. These are used to a guide when attempting to repair
#' \code{x}/\code{by} combinations with multiple values associated. See Details
#' below

#' @details
#' \code{addTubeMeta} attempts to merge \code{data} and \code{ref} using
#' \code{by} as the common merging-term. It is intended for use with reference
#' meta data or data extracted with from a reliable source using
#' \code{extractTubeMeta}.
#'
#' \code{checkTubeMeta} attempts to identified all unique values for an
#' \code{x}/\code{by} combination. Meta-data is expected to unique at a
#' specific level of aggregation, e.g. all \code{latitude} records should
#' be identical for a given sample site. So, finding multiple values
#' and/or unexpected values as options for \code{x} suggests the data-series
#' is not meta-data at that level or it has been corrupted. (See also
#' \code{repairTubeMeta} below.)
#'
#' \code{extractTubeMeta} attempts to extract data that looks like meta
#' information from the supplied \code{data}.
#' Although the function default is to look for known sample identifiers,
#' this grouping term can be set using the addition argument \code{by}.
#' The function then extracts all data-series with only one unique
#' non-\code{NA} value for case of \code{by}. By default, the function
#' tests all non-grouping data-series, but similarly testing/extraction
#' can be limited to specific data-series using \code{x}. Please remember that
#' this function extracts data-series that looks like meta information at
#' the \code{by} grouping-level. There is no unambiguous test to identify a
#' data-series as meta-data. So, this function needs to be handled with care,
#' especially if you have any concerns about the quality of your data sets.
#'
#' \code{extractAndAddTubeMeta} is a wrapper for \code{extractTubeMeta} and
#' \code{addTubeMeta} to extract valid meta-information from a source,
#' \code{ref}, and add that meta-information to \code{data}. similarly, care
#' should be taken using this function.
#'
#' \code{padTubeMeta} attempts to pad the \code{x} data-series by replacing
#' any \code{NA}s with the first non-\code{NA} entry from the same data-series.
#' It is intended for use with meta-data data-series, e.g. a \code{latitude}
#' (or \code{longitude column}) where value was only entered once. Again, care
#' should be taken using this function.
#'
#' \code{repairTubeMeta} attempts to repair what looks like bad
#' meta-information. It uses an extra argument \code{options}, a list of
#' valid options for \code{x}, as a reference to rationalise multiple value
#' \code{by} subsets.
#'

# will need to document this further if it stays...

#' @return These functions are generally intended to be used in the form:
#'
#' \code{updated.data <- addTubeMeta(dt.data, ref, "[by.name]")}
#'
#' \code{requested.data <- checkTubeMeta(dt.data, "[meta.name]", "[by.name]")}
#'
#' ... etc
#'
#' \code{addTubeMeta} returns the supplied \code{data} with \code{ref}
#' merged at the requested level.
#'
#' \code{checkTubeMeta} returns a \code{data.frame} of values associated with
#' found when grouping \code{x} data at the requested level. More than one
#' value and/or unexpected values indicate that \code{x} may not be meta-data
#' or, if it is, that the meta-data has been corrupted.
#'
#' \code{extractTubeMeta} returns a \code{data.frame} of data that looks like
#' meta-data when grouped at the requested level. (NB: data-series have to be
#' unique at the grouping level requested to generate an output with
#' \code{extractTubeMeta}. So, any \code{x}/\code{by} combinations that
#' \code{checkTubeMeta} reports as having multiple outputs will be ignored by
#' this function.)
#'
#' \code{padTubeMeta} returns \code{data} with the requested fix,
#' if it can be applied.
#'
#' \code{repairTubeMeta} like \code{padTubeMeta}, returns the fixed \code{data},
#' if the repair can be made.



######################################
# to think about
######################################

# pad/repairTubeMeta
#    should probably drop padTubeMeta or replace with a
#        repairTubeMeta wrapper (see notes)

# extractTubeMeta
#     looks like function might be killing .start_date, .end_date, etc
#     could be from move from dplyr to data.table...
#           example
#           head(extractTubeMeta(dt.brd, by=".date"))




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

# need by because
#   it strips all non-by data-series in ref from data before merging...

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

# check the checkTube(s) doing x but not by....
#     but think at least some of the should be
#     merged in require-tags-only variation on tagTube
#     or allow for a dummyTube() function that allows you to
#     pass untagged files to a tube function (at you own risk)
# (same for repairTubeMeta)

# notes
#############################


#' @rdname misc.tube.meta
#' @export

checkTubeMeta <- function(data, x=NULL, by=NULL, ...){

  #setup
  .d <- tagTube(data)
  .xargs <- list(...)
  if(is.null(x) | is.null(by)){
    stop("[checkTubeMeta] need both 'x' and 'by'",
         call.=FALSE)
  }
  .d <- checkTubeData(.d, x, if.err="stop<<checkTubeMeta>>x")
  #####################
  # should be a better way of doing b=next bit...
  if(by==".location"){
    .d <- tagTubeLocation(.d)
  }
  if(by==".date"){
    .d <- tagTubeDate(.d)
  }

  #main check
  out <- calcTubeStat(.d, x, by,
                      function(x) {list(
                        count = length(unique(x, na.rm=FALSE)),
                        options = paste("'", sort(unique(x, na.rm=FALSE)), "'",
                                        sep="", collapse = "|")
                      )})
  out <- out[order(out[,paste(x, ".count", sep="")], decreasing = TRUE),]
  #output
  if("output" %in% names(.xargs)){
    # full report
    if(tolower(.xargs$output) %in% c("report", "full.report")){
      return(out)
    }
  }
  # summary report
  # think about labelling
  #    this is option.count, options (as string), and number of locations
  #    this NEEDS calcTubeStat to work with datasets that are NOT tagged
  calcTubeStat(out, tube = by, by = paste(x, c("count", "options"), sep="."),
               stat=function(x) list(length(x)))

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
  data <- tagTubeRequired(data, required=c(x, by), ...)
  if (is.null(by)) {
    if (".sample_id" %in% names(data)) {
      by <- ".sample_id"
    } else {
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
# extractAndAddTubeMeta
#############################

# in development

# this is a wrapper to meta info from a ref and add it to data
#   used to re-add the meta after a function strips it...

# notes
#############################

# to think about
#############################

# this could go into functions like fitTubeModel
#     as a final step repair before sending back???
#      BUT it might need careful texting because missuse could kill the data


#' @rdname misc.tube.meta
#' @export

extractAndAddTubeMeta <- function(data, x=NULL, by=NULL, ref=NULL, ...){

  # I think this has to be step-wise BUT should test???
  # do we option to strip .meta or data ???
  #     i think current mechanism is
  #         any duplicate columns are stripped from data before adding from ref
  #            need to check ???
  #     could do either here and/or in one/both of extract... and/or add...
  #     maybe something like trust="data"/"ref"
  for(i in 1:length(by)){
    .meta <- extractTubeMeta(ref, by=by[i], x=x, ...)
    data <- addTubeMeta(data, ref=.meta, by=by[i], ...)
  }
  return(data)
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





###############################
# repairTubeMeta
###############################

# might want to check through all getTubeX/checkTubeData handling

# this may need careful documentation...

# currently only one repair method

# options = all known valid options for x.
# functions assumes x is meta-information at the by-aggregated level
#    AND that all valid options are known
#        example: options = c('Valid', 'Invalid')
#           for non-unique level 'Valid' | '' | 'Unknown' => 'Valid'
#           for unique level 'Valid' => 'Valid' [or strictly no change]
#           for non-unique level 'Valid' | 'Invalid' => [suffix all '.SUSPECT']
#           for non-unique level '' | 'Unknown' => [no changes]
#           for non-unique level 'Invalid' | '' | 'Unknown' => 'Invalid'
#           etc ...

# caveat
# this assumes that e.g. location based meta-info does not change, e.g. a Valid
#     site does not become Invalid...

# notes
#############################


# think about
############################

# function options argument is currently specific....
#     do we want to drop to making all lower case for this...?

# this might be better/safer than current padTubeMeta
#   I think repairTubeMeta(data, x, by, options=unqiue(data[[x]]))
#        would do similar but with not use first unique(x) if there
#        were multiple... (NB: pad... uses unique(x)[1]... )

# might want option to silence warnings/messages

#' @rdname misc.tube.meta
#' @export

repairTubeMeta <- function(data, x=NULL, by=NULL, options=NULL, ...){

  #setup
  if(is.null(options)){
    stop("[repairTubeMeta] need 'options' to work with...",
         call.=FALSE)
  }
  .d <- tagTube(data)
  .xargs <- list(...)
  if(is.null(x) | is.null(by)){
    stop("[repairTubeMeta] need both 'x' and 'by'",
         call.=FALSE)
  }
  .d <- checkTubeData(.d, x, if.err="stop<<checkTubeMeta>>x")
  #####################
  # should be a better way of doing b=next bit...
  if(by==".location"){
    .d <- tagTubeLocation(.d)
  }
  if(by==".date"){
    .d <- tagTubeDate(.d)
  }

  #main loop
  ###################################
  # Keep an eye on the speed of this
  # thought it would a lot slower
  #    BUT may still slow down dramatically
  #    on very big data.frames
  .ref <- unique(.d[[by]])
  .n.rep <- c(0,0)
  for(i in .ref){
    .ops <- sort(unique(.d[.d[[by]]==i, x]))
    .ts <- options[options %in% .ops]
    if(length(.ts)==1 & length(.ops)>1){
      # have a valid option to update
      # (and something that needs updating)
      .d[.d[[by]]==i, x] <- .ts
      .n.rep[1] <- .n.rep[1] + 1
    }
    if(length(.ts)>1){
      # have multiple valid options, tag as suspect
      .d[.d[[by]]==i, x] <- paste(.d[.d[[by]]==i, x], ".SUSPECT", sep="")
      .n.rep[2] <- .n.rep[2] + 1
    }
  }
  if(.n.rep[1]>1 | .n.rep[2]>1){
    message("[repairTubeMeta] ", .n.rep[1], " repair(s) made; ", .n.rep[2],
            " suspect(s) subsets identified.")
  }
  return(.d)

}



