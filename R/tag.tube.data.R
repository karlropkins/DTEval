############################################
#' @title Diffusion Tube Pro-processing
############################################

#' @name tag.tube.data
#' @aliases tag.tube.data tagTube tagTubeDates tagTubeLatLon tagTubeSiteID

#' @description Pre-processing diffusion tube (DT) data for use with
#' \code{DTEval}. Coded methods to standardise DT data collected using
#' different DT sampling procedures or different logging conventions.
#' Tagging makes a version of your tube information in a format
#' \code{DTEval} can more easily work with, while retaining your original
#' data in the format you are familiar with.
#'
#' All \code{tag...} functions are intended to be run in the form:
#'
#' \code{tagged.data <- tagTube(data, ...)}
#'
#' (where ... refers to any additional tagging arguments)

# basically, however the data comes in, we tag it and then work with
# the tags as much as possible afterwards ... then if anything changes
# or file-to-file, client-to-client, or source.file-vs-database.export, etc,
# the chances of it killing routine analysis methods are reduced ...

# Well, might work ... ??

#' @param data Data source, typically a
#' data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param method The method to apply when testing or modifying \code{data}.
#' The default (\code{method=-1}) means try all methods in order until one
#' works. See below for further details on different function methods.
#' @param force (logical) By default, these functions do not overwrite
#' existing tags if they are already present. The option \code{force=TRUE}
#' forces them to rebuild the requested tag using current setting.
#' @param ... Additional arguments can be used to assign data sources if
#' these are not automatically identified by the \code{tag...} functions.
#' See below for further details.

#' @return These functions typically return the supplied \code{data}
#' modified as requested.

#' @details All \code{tag...} functions allow you to change information sources when
#' adding tags. \code{DTEval} has defaults it is expects, but your files
#' might be different. So, you can identify the specific \code{data} columns
#' you would like used using additional arguments, e.g.:
#'
#' * \code{year} to assign the calendar year
#'
#' * \code{month} to assign the calendar month
#'
#' * \code{start} and \code{end} to assign the individual DT
#'  sampling period directly (\code{start} deployment and \code{end}
#'  collection, respectively). \code{start} and \code{stop} are assumed
#'  to be provided in conventional 'YYYY-MM-DD' format, but this can also
#'  be modified using \code{data.format} and \code{\link{strptime}}
#'  formatting instructions.
#'
#'  * \code{lat} and \code{lon}  to assign latitude and longitude, respectively.
#'
#' So, for example:
#'
#' \code{tagTubeLatLon(data, lat="lat", lon="long")}
#'
#' tells \code{tagTubeLatLon} to use the \code{data} columns named 'lat' and
#' 'long' as the sources when tagging latitude and longitude, respectively.
#'
#' Function Methods:
#'
#' By default, these methods try all available methods in order, until they
#' find a method that works and they run out methods. If you want to use a
#' specific method, or method order, just specific them, e.g. to tag tube
#' dates using first \code{method} 3 then 2:
#'
#' \code{tagTubeDates(data, method=3:2)}
#'
#' \code{tagTubeDates} attempts to build \code{Date} class time-stamps for
#' sampling dates. It currently has three methods: \code{method=1} use month
#' and year records to build a nominal calendar monthly sampling record,
#' i.e., first to last day of each month; \code{method=2} use month and year
#' records to build a sampling calendar based on the DEFRA diffusion tube
#' sampling calendar (see \code{\link{dt.calendar}}); and, \code{method=3}
#' to build directly using user-assigned sampling \code{start} and
#' \code{end} dates. If sampling dates are built, these are added to
#' \code{data} as columns \code{.start_date} and \code{.end_date}.
#'
#' \code{tagTubeLatLon} identifies sample location. It currently uses
#' one method: \code{method=1} assigning all locations based on identified
#' latitude and longitude. These are then tagged as \code{.latitude} and
#' \code{.longitude}, respectively.

# NEED TO document required LAT/LON conventions...
# ALSO time allowing what to include so transform options here ...

#'
#' \code{tagTubeSiteID} identifies replicate samples. It currently uses
#' one method: \code{method=1} assigning all same-location-and-time DTs as
#' replicates, and is added to \code{data} as \code{.site_id}.
#'
#' \code{tagTube} is a wrapper that runs \code{tagTubeDates},
#' \code{tagTubeLatLon} and \code{tagTubeiteID}. Other \code{DTEval}
#' functions typically pass your \code{data} to \code{tagTube} to check for
#' tags and add them if missing. If you are using \code{tagTube} or one of
#' these on untagged data and need to specific individual methods for each
#' \code{tagTube...} function, prefix the method request, e.g. to use method
#' 2 for \code{tabTubeDates} but leave the other functions using default
#' setting when applying \code{tagTube}, use:
#'
#' \code{tagTube(data, dates.method=2)}
#'

#################################
# general
#################################

# JOBS:

# stops/warns/messages need rationalising
#      start with this functions and then apply more widely

# some code wants tidying
#      if tags there and not forcing, return should be first
#          no point wasting time doing anything
#          currently first in latlon and siteid but NOT dates
#

################################
# tagTube
################################

# in development wrapper for main diffusion tube info tagging
# applies in order tagTubeDates, tagTubeLatLon, tagTubeSiteId

# used by testTubePrecision



#' @rdname tag.tube.data
#' @export
tagTube <-
  function(data, ...){

    #note
    ############################
    # to override for individual functions (force or method only)
    #    e.g. dates.force overrides force for just tagTubeDates, etc...
    #    logic: then always there, e.g. in tagTubeSiteID when it run
    #           other two...

    data <- tagTubeDates(data, ...)
    data <- tagTubeLatLon(data, ...)
    data <- tagTubeSiteID(data, ...)
    return(data)
  }


################################
# tagTubeDates
################################

# in development at moment
# diffusion tube date tagging

# has thee methods (as unexported functions, after main function)
# 1. default monthly
# 2. Defra/laqm scheme only works for monthly sampling
# 3. specific start and end dates

#' @rdname tag.tube.data
#' @export
tagTubeDates <-
  function(data, method = -1, force=FALSE, ...){

    # setup/checks
    .xargs <- list(...)
    if("dates.force" %in% names(.xargs)){
      force <- .xargs$dates.force
    }
    if("dates.method" %in% names(.xargs)){
      method <- .xargs$dates.method
    }

    #methods
    fun.ls <- list(tagTubeDates_method01,
                   tagTubeDates_method02,
                   tagTubeDates_method03)
    check <- 1:length(fun.ls)
    if(all(method == -1)) {
      method <- check
    } else {
      if(!all(method %in% check)){
        stop("[tagTubeDate]> unknown method(s) '",
             paste(method[!method %in% check], collapse = "',"), "'",
             "\n\trecommend one of: ", paste (check, collapse = ", "),
             "\n\t(and maybe check ?tagTubeSiteID) \n",
             call.=FALSE)
      }
    }
    fun.ls <- fun.ls[method]
    #main
    if(!(".start_date" %in% names(data) & ".end_date" %in% names(data)) | force){
      for(i in 1:length(fun.ls)){
        if(i==1 || class(ans)[[1]]=="try-error"){
          ans <- try(fun.ls[[i]](data, ...), silent=TRUE)
        }
      }
      if(class(ans)[1]=="try-error"){
        stop("[tagTubeDates]> failed to match/build dates",
             "\n\t(maybe check ?tagTubeDates) \n",
             call.=FALSE)
      }
      data <- ans
    }
    #out
    return(data)
  }


# unexported tube date build models

tagTubeDates_method01 <- function(data, ...){
  #dates build method 1
  ########################
  #nominal calendar month from month and year
  .xargs <- modifyList(list(month="month",
                            year="year_of_measurement"),
                       list(...))
  mon <- data[,.xargs$month]
  # assuming mon is
  # numeric only 1 to 12
  # or character
  #     either only 3 letter abbrevs
  #            or only full names
  # and setting date format
  # NOTES
  # this does not catch month 1:12 if character or months that are
  #         not 3-letter abrevs or full names
  #         BUT seems to handle character.month case (under/lower) variations...
  #         (should look into how it does that, see method 02)
  frmt <- if(is.numeric(mon)){
    "%d/%m/%Y"
  } else {
    #make local version character (might be factor)...
    mon <- as.character(mon)
    temp <- nchar(mon)[!is.na(nchar(mon))]
    if(all(temp==3)) {
      "%d/%b/%Y"
    } else {
      "%d/%B/%Y"
    }
  }
  yr <- as.numeric(data[,.xargs$year])
  temp <- as.POSIXlt(paste("01", mon, yr, sep="/"),
                            format=frmt)
  if(all(is.na(temp))) { stop() }
  data$.start_date <- as.Date(temp)
  temp$mon <- temp$mon + 1
  temp <- as.Date(temp) - 1
  data$.end_date <- temp
  data
}

tagTubeDates_method02 <- function(data, ...){
  #dates build method 2
  ##################################
  # using month, year and
  # defra/laqm calendar month (using dt.calendar)
  .xargs <- modifyList(list(month="month",
                            year="year_of_measurement"),
                       list(...))
  temp <- DTEval::dt.calendar[c("year", "month", "start", "end")]
  temp$year <- as.numeric(temp$year)
  # like method 01
  # but may also fall over on case of character
  # and reshaping temp/dt.calendar ahead on join...
  temp$month <- if(is.numeric(data[, .xargs$month])){
    match(month.name, temp$month)
  } else {
    #assuming all names or common abbrevs
    #AND pushing factors, etc, to character
    #    would rather keep original as-was like in method01
    #    (problem when I have more time...)
    data[, .xargs$month] <- as.character(data[, .xargs$month])
    test <- nchar(data[, .xargs$month])[!is.na(nchar(data[, .xargs$month]))]
    # messy but some in db data sets have missing months
    if(all(test[test!=0]==3)){
      temp$month <- substr(temp$month, 1, 3)
    }
  }
  names(temp) <- c(.xargs$year, .xargs$month, ".start_date", ".end_date")
  if(all(c(.xargs$month, .xargs$year) %in% names(data))){
    data <- dplyr::left_join(data, temp,
                             by = c(.xargs$year, .xargs$month))
  } else { stop() }
  data
}

tagTubeDates_method03 <- function(data, ...){
  #dates build method 3
  ##################################
  # using set start and end
  .xargs <- modifyList(list(start=".start_date",
                            end=".end_date",
                            date.format="%Y-%m-%d"),
                       list(...))
  if(all(c(.xargs$start, .xargs$end) %in% names(data))){
    data$.start_date <- if(class(data[, .xargs$start])[1]=="Date"){
       data[, .xargs$start]
    } else {
      as.Date(as.character(data[, .xargs$start]),
              format=.xargs$date.format)
    }
    data$.end_date <- if(class(data[, .xargs$end])[1]=="Date"){
      data[, .xargs$end]
    } else {
      as.Date(as.character(data[, .xargs$end]),
              format=.xargs$date.format)
    }
  } else stop()
  return(data)
}


################################
# tagTubeLatLon
################################

# used by tagTubeSiteID

# sim here is handle lat lon transforms here if we have time to include it

#' @rdname tag.tube.data
#' @export
tagTubeLatLon <-
  function(data, method = -1, force=FALSE, ...){

    #return if lat and lon already tagged and not forcing ...
    if(".latitude" %in% names(data) & ".longitude" %in% names(data) &
       !force){
      return(data)
    }
    #setup
    .xargs <- modifyList(list(lat="latitude",
                              lon="longitude"),
                         list(...))
    #check lat and lon are in data
    test <- c()
    for(i in c("lat", "lon")){
      if(!.xargs[[i]] %in% names(data)){
        test <- c(test, paste("'", .xargs[[i]], "' (", i, ")", sep="", callapse=""))
      }
    }
    if(length(test) >0){
      stop("[tagTubelatlon]> expected latlon source(s) not in data\n\t",
           "missing: ", paste(test, sep=", ", collapse = ","),
           "\n\t(maybe check data or ?tagTubeLatLon) \n",
           call.=FALSE)
    }
    # higher level force and method override
    if("latlon.force" %in% names(.xargs)){
      force <- .xargs$latlon.force
    }
    if("latlon.method" %in% names(.xargs)){
      method <- .xargs$latlon.method
    }
    #check method
    check <- 1
    if(all(method == -1)){
      method <- check
    }
    if(!method %in% check){
      stop("[tagTubeLatLon]> method ", method, " not known",
           "\n\trecommed one of: ", paste (check, collapse = ", "),
           "\n\t(and maybe check ?tagTubeLatLon) \n",
           call.=FALSE)
    }

    #tag
    if(method=="1"){
      #sources exist
      data$.latitude <- data[, .xargs$lat]
      data$.longitude <- data[, .xargs$lon]
      ########################
      #checking, formatting, etc to go here if time/interest in doing
      ########################
    }
    data
  }






################################
# tagTubeSiteID
################################

# used by tubeReplicatePrecision

# this step is currently on month, year_of_measurement and latitude and
# longitude ....
# could be more robust ....

#' @rdname tag.tube.data
#' @export
tagTubeSiteID <-
  function(data, method = -1, force=FALSE, ...){

    #fast return if siteid there and not forcing...
    if(".site_id" %in% names(data) & !force){
      return(data)
    }
    #setup
    data <- tagTubeDates(data, ...)
    data <- tagTubeLatLon(data, ...)
    # higher level force and method overwrites
    .xargs <- list(...)
    if("siteid.force" %in% names(.xargs)){
      force <- .xargs$siteid.force
    }
    if("siteid.method" %in% names(.xargs)){
      method <- .xargs$siteid.method
    }
    #check method
    check <- 1
    if(all(method == -1)){
      method <- check
    }
    if(!method %in% check){
      stop("[tagTubeSiteID]> '", method, "' unknown method",
           "\n\trecommend one of: ", paste (check, collapse = ", "),
           "\n\t(and maybe check ?tagTubeSiteID) \n",
           call.=FALSE)
    }
#########################
# old method
# DEFRA DTs ONLY
#########################
#    if(method=="1"){
#      #match latitude+longitude
#      test <- factor(paste(data$latitude, data$longitude,
#                           data$year_of_measurement, data$month))
#      data$.site_id <- as.numeric(test)
#    }
    if(method=="1"){
      #match latitude+longitude
      test <- data[c(".latitude", ".longitude", ".start_date", ".end_date")]
      test <- as.factor(apply(test , 1 , paste , collapse = "-" ))
      data$.site_id <- as.numeric(test)
    }
    data
  }





