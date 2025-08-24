###########################
#misc setup and code
###########################

#' @importFrom stats as.formula end lm loess predict qt quantile sd start qnorm na.omit median
#' @importFrom utils data modifyList
#' @importFrom methods is
#' @importFrom grDevices colorRampPalette

##########################
# don't want full import
##########################
## #' @import data.table
#' @importFrom data.table as.data.table .SD :=




#undefined globals
utils::globalVariables(c(".data", ".mean",".n", ".tube",".y", ".yhigh",
                         ".ylow", "latitude", "longitude", ".", ".value",
                         ".latitude", ".longitude", ".start_date",
                         ".end_date", "X", "Y", "checksum"))


###########################
#currently unexported
###########################

#lookup table like openair but for
#based on aqe_quickText in AQEval (also unexported)

dte_quickText <- function (text, auto.text = TRUE)
{
  if (!auto.text)
    return(ans <- text)
  ans <- text
  ans <- gsub("NO2", "NO<sub>2</sub>", ans)
  ans <- gsub("no2", "NO<sub>2</sub>", ans)
  ans <- gsub("NOX", "NO<sub>x</sub>", ans)
  ans <- gsub("nox", "NO<sub>x</sub>", ans)
  ans <- gsub("NOx", "NO<sub>x</sub>", ans)
  ans <- gsub("NH3", "NH<sub>3</sub>", ans)
  ans <- gsub("nh3", "NH<sub>3</sub>", ans)
  ans <- gsub("co ", "CO ", ans)
  ans <- gsub("co,", "CO,", ans)
  ans <- gsub("nmhc", "NHHC", ans)
  ans <- if (nchar(as.character(text)) == 2 && length(grep("ws",
                                                           text)) > 0) {
    gsub("ws", "wind spd.", ans)
  }
  else {
    ans
  }
  ans <- gsub("wd", "wind dir.", ans)
  ans <- gsub("rh ", "relative humidity ", ans)
  ans <- gsub("PM10", "PM<sub>10</sub>", ans)
  ans <- gsub("pm10", "PM<sub>10</sub>", ans)
  ans <- gsub("pm1", "PM<sub>1</sub>", ans)
  ans <- gsub("PM1", "PM<sub>1</sub>", ans)
  ans <- gsub("PM4", "PM<sub>4</sub>", ans)
  ans <- gsub("pm4", "PM<sub>4</sub>", ans)
  ans <- gsub("PMtot", "PM<sub>total</sub>", ans)
  ans <- gsub("pmtot", "PM<sub>total</sub>", ans)
  ans <- gsub("pmc", "PM<sub>coarse</sub>", ans)
  ans <- gsub("pmcoarse", "PM<sub>coarse</sub>", ans)
  ans <- gsub("PMc", "PM<sub>coarse</sub>", ans)
  ans <- gsub("PMcoarse", "PM<sub>coarse</sub>", ans)
  ans <- gsub("pmf", "PM<sub>fine</sub>", ans)
  ans <- gsub("pmfine", "PM<sub>fine</sub>", ans)
  ans <- gsub("PMf", "PM<sub>fine</sub>", ans)
  ans <- gsub("PMfine", "PM<sub>fine</sub>", ans)
  ans <- gsub("PM2.5", "PM<sub>2.5</sub>", ans)
  ans <- gsub("pm2.5", "PM<sub>2.5</sub>", ans)
  ans <- gsub("pm25", "PM<sub>2.5</sub>", ans)
  ans <- gsub("PM2.5", "PM<sub>2.5</sub>", ans)
  ans <- gsub("PM25", "PM<sub>2.5</sub>", ans)
  ans <- gsub("pm25", "PM<sub>2.5</sub>", ans)
  ans <- gsub("O3", "O<sub>3</sub>", ans)
  ans <- gsub("o3", "O<sub>3</sub>", ans)
  ans <- gsub("ozone", "O<sub>3</sub>", ans)
  ans <- gsub("CO2", "CO<sub>2</sub>", ans)
  ans <- gsub("co2", "CO<sub>2</sub>", ans)
  ans <- gsub("SO2", "SO<sub>2</sub>", ans)
  ans <- gsub("so2", "SO<sub>2</sub>", ans)
  ans <- gsub("H2S", "H<sub>2</sub>S", ans)
  ans <- gsub("h2s", "H<sub>2</sub>S", ans)
  ans <- gsub("CH4", "CH<sub>4</sub>", ans)
  ans <- gsub("ch4", "CH<sub>4</sub>", ans)
  ans <- gsub("dgrC", "<sup>o</sup>C", ans)
  ans <- gsub("degreeC", "<sup>o</sup>C", ans)
  ans <- gsub("deg. C", "<sup>o</sup>C", ans)
  ans <- gsub("degreesC", "<sup>o</sup>C", ans)
  ans <- gsub("ug/m3", "&mu;g.m<sup>-3</sup>", ans)
  ans <- gsub("ug.m-3", "&mu;g.m<sup>-3</sup>", ans)
  ans <- gsub("ug m-3", "&mu;g.m<sup>-3</sup>", ans)
  ans <- gsub("ugm-3", "&mu;g.m<sup>-3</sup>", ans)
  ans <- gsub("mg/m3", "mg.m<sup>-3</sup>", ans)
  ans <- gsub("mg.m-3", "mg.m<sup>-3</sup>", ans)
  ans <- gsub("mg m-3", "mg.m<sup>-3</sup>", ans)
  ans <- gsub("mgm-3", "mg.m<sup>-3</sup>", ans)
  ans <- gsub("ng/m3", "ng.m<sup>-3</sup>", ans)
  ans <- gsub("ng.m-3", "ng.m<sup>-3</sup>", ans)
  ans <- gsub("ng m-3", "ng.m<sup>-3</sup>", ans)
  ans <- gsub("ngm-3", "ng.m<sup>-3</sup>", ans)
  ans <- gsub("m/s2", "m.s<sup>-2</sup>", ans)
  ans <- gsub("m/s", "m.s<sup>-1</sup>", ans)
  ans <- gsub("m.s-1", "m.s<sup>-1</sup>", ans)
  ans <- gsub("m s-1", "m.s<sup>-1</sup>", ans)
  ans <- gsub("g/km", "g.km<sup>-1</sup>", ans)
  ans <- gsub("g/s", "g.s<sup>-1</sup>", ans)
  ans <- gsub("kW/t", "kW.t<sup>-1</sup>", ans)
  ans <- gsub("g/hour", "g.hour<sup>-1</sup>", ans)
  ans <- gsub("g/hr", "g.hour<sup>-1</sup>", ans)
  ans <- gsub("g/m3", "g.m<sup>-3</sup>", ans)
  ans <- gsub("g/kg", "g.kg<sup>-1</sup>", ans)
  ans <- gsub("km/hr/s", "km.hour<sup>-1</sup>s<sup>-1</sup>",
              ans)
  ans <- gsub("km/hour/s", "km.hour<sup>-1</sup>s<sup>-1</sup>",
              ans)
  ans <- gsub("km/h/s", "km.hour<sup>-1</sup>s<sup>-1</sup>",
              ans)
  ans <- gsub("km/hr", "km.hour<sup>-1", ans)
  ans <- gsub("km/h", "km.hour<sup>-1", ans)
  ans <- gsub("km/hour", "km.hour<sup>-1", ans)
  ans <- gsub("r2", "R<sup>2", ans)
  ans <- gsub("R2", "R<sup>2", ans)
  ans <- gsub("\n", "<br>", ans)
  ans
}




###########################
# dte_ggshellTestArgs
###########################

# might export this later???

# test data and .xargs for use with ggplot2...

# was dte_testShellAndArgs

dte_ggshellTestArgs <- function(.xargs, data){
  .xargs.tmp <- lapply(.xargs, function(x) !is.null(getTubeX(data, x)))
  .xargs.source <- ifelse(.xargs.tmp, "data", "unknown")
  .xargs.tmp <- lapply(.xargs, function(x) !is.null(getTubeX(NULL, x)))
  .xargs.source <- ifelse(.xargs.tmp, "fact", .xargs.source)
  .xargs.source <- as.list(.xargs.source)
  names(.xargs.source) <- names(.xargs)
  .xargs.source
}



################################
# dte_addGeomToShell
################################

# tubePlot handler

# was dte_ggplotAddGeom

dte_ggshellAddGeom <- function(.xargs, data, ggplot,
                              geom, defaults=NULL, holds=NULL, drops=NULL){
  # .xargs - list; what we are passing to the geom
  # data - data source
  # ggplot is the shell output
  # geom is the geom we are adding
  # defaults - list;any defaults you want geom to apply
  # holds - list of values we want to keep
  # drops - vector; names of anything we don't want given to the geom
  if(is.null(defaults)){
    defaults=list()
  }
  if(is.null(holds)){
    holds=list()
  }
  if(is.null(drops)){
    drops=c()
  }
  .xargs <- modifyList(defaults, .xargs)
  .xargs <- modifyList(.xargs, holds)
  .test <- dte_ggshellTestArgs(.xargs, data)
  .test <- .test[!names(.test) %in% drops]
  xx <- ggplot$mapping
  rest <- list()
  if(length(.test)>0){
    for(i in 1:length(.test)){
      if(.test[[i]] == "data"){
        xxx <- ggplot2::aes(temp = .data[[{{.xargs[[names(.test)[i]]]}}]])
        names(xxx) <- names(.test)[i]
        xx <- modifyList(xx, xxx)
      } else {
        rest <- c(rest, .xargs[names(.test)[i]])
      }
    }
  }
  ggplot + do.call(geom, modifyList(list(mapping=xx, data=data), rest))
}




##########################
# testing removing...
##########################

# old function
# now using tagTubeSampleID which is a little different

#findTubeReplicates <-
#  function(data, method = 0){
#    #setup
#    if(method %in% 0:1){
#
#      ## switch might be neater...
#
#      ## each method should:
#      ## make replicate and site id identifiers
#      if(method==0){
#        #match latitude+longitude
#        test <- factor(paste(data$latitude, data$longitude,
#                             data$year_of_measurement, data$month))
#        data$site_id <- as.numeric(test)
#        data$replicate <- 1
#      }
#      if(method==1){
#        # banes-style duplication
#        # tube id DT[NUM], DT[NUM]b, DT[NUM]c.
#        test <- tolower(data$site)
#        ref <- rep("1", nrow(data))
#        ref[grep(".b$", test)] <- "2"
#        ref[grep(".c$", test)] <- "3"
#        ref[grep(".d$", test)] <- "4"   #not expecting any
#        data$replicate <- ref
#        data$site_id <- gsub("b$|c$", "",data$site)
#      }
#
#    } else {
#      stop("Unknown method")
#    }
#    #test <- as.data.table(data)
#    #test <- test[, .(
#    #  .n = length(replicate),
#    #  .mean = mean(measurement, na.rm=TRUE),
#    #  .bc.mean = mean(bias_adjusted_measurement, na.rm=TRUE)
#    #), by = c("year_of_measurement", "month", "site_id")]
#    #test <- as.data.frame(test)
#    #test <- subset(test, .n==3)
#    #test <- data.table::merge.data.table(as.data.table(data),
#    #                             as.data.table(test),
#    #                             all.x=FALSE, all.y=TRUE)
#
#    #as.data.frame(test)
#    test <- group_by(data, year_of_measurement, month, site_id)
#    test <- summarise(test,
#                      .n = length(replicate),
#                      .mean = mean(measurement, na.rm=TRUE),
#                      .bc.mean = mean(bias_adjusted_measurement, na.rm=TRUE)
#    )
#    test <- merge(data, test)
#
#
#
#    test
#  }


#dte_check.data <- function(data){
#  #columns we expect
#  .cols <- c("year_of_measurement", # the year of measurement
#             "month",               # the month, three letter abbrev
#             "month_numeric"        # the month as a numeric
#  )
#  test <- .cols[!.cols %in% names(data)]
#  if(length(test)>0){
#    warning(paste("missing: ", paste(test, sep="", collapse = ", "), sep=""))
#  }
#  invisible(data)
#}

#dte_add.date <- function(data, tz=""){
#  temp <- paste(data$`year_of_measurement`,
#                data$`month_numeric`,
#                "01", sep="-")
#  if(tz==""){
#    #should we warn?
#    tz <- format(Sys.Date(), "%Z")
#  }
#  data$date <- suppressWarnings(as.POSIXct(temp,
#                                           format = "%Y-%m-%d", tz = tz))
#  data
#}


###########################################
## NOT exporting
## MAYBE NOT keeping either of the aggregate functions below.
###########################################

## notes
##> aggregateCALikeTube(ref, banes, method=2)
##Joining with `by = join_by(start.date, end.date)`
##(multiple times)
##
##   could think about add to method 1 join ???
##   in calcDateRangeStat in AQEval

#aggregateCALikeTube <- function(ref, data, agg.method=1, ...){
#
#  data <- tagTube(data, ...)
#  #get the unique .start_date/.end_end combinations
#  data <- data[!duplicated(paste(data$.start_date, data$.end_date)),]
#  if(agg.method==1){
#    out <- AQEval::calcDateRangeStat(ref, from=data$.start_date,
#                                     to=data$.end_date, method=2)
#  }
#  if(agg.method==2){
#    out <- AQEval::calcDateRangeStat(ref, from=data$.start_date,
#                                     to=data$.end_date, method=1)
#  }
#  names(out)[1:2] <- c(".start_date", ".end_date")
#  out

#}


#aggregateCALikeTube.old <- function(ref, method=1){
#
#  #rethinking this after introducting the tag method
#  #this is the old DEFRA TUBES only method ....
#
#  #for ca data
#  #note
#  #########################
#  #could also have a method 3
#  #  give us your own calendar
#  ########################
#
#  data <- ref
#  method <- as.character(method)
#  .check <- 1:2 # available methods
#  if(!method %in% .check){
#    stop("aggregateCALikeTube: '", method, "' unknown method",
#         "\n\trecommend one of: ", paste (.check, collapse = ", "),
#         "\n\t(and maybe check ?aggregateCALikeTube) \n",
#         call.=FALSE)
#  }
#  if(method==1){
#    #method: use standard calendar months
#    # little minimal at the moment
#    # also loosing a lot of meta
#    #    may want to add some back in...???
#    data$year_of_measurement <- as.numeric(format(data$date, "%Y"))
#    data$month <- format(data$date, "%b")
#    out <- dplyr::group_by(data, year_of_measurement, month, site)
#    out <- dplyr::summarise_if(out, is.numeric, mean, na.rm = TRUE)
#  }
#  if(method=="2"){
#    #method: use-defra-sampling calendar
#    out <- AQEval::calcDateRangeStat(data, from=dt.calendar$start,
#                                     to=dt.calendar$end, method=2)
#    names(out)[names(out)=="start.date"] <- "start"
#    out$start <- as.Date(out$start, format="%Y-%m-%d")
#    names(out)[names(out)=="end.date"] <- "end"
#    out$end <- as.Date(out$end)
#    #########################################
#    # this next bit feels a little messy
#    # could be an option in optional out in calcData...
#    out <- dplyr::left_join(out, dt.calendar, by = join_by(start, end))
#    out$month <- substr(out$month, 1, 3)
#    names(out)[names(out)=="year"] <- "year_of_measurement"
#  }
#
#  invisible(out)
#}




