###########################
#misc setup and code
###########################

#' @importFrom stats as.formula end lm loess predict qt quantile sd start qnorm na.omit median cor
#' @importFrom utils data modifyList
#' @importFrom methods is as
#' @importFrom grDevices colorRampPalette as.raster grey hcl

##########################
# don't want full import
##########################
## #' @import data.table
#' @importFrom data.table as.data.table .SD :=


#undefined globals
utils::globalVariables(c(".data", ".mean",".n", ".tube",".y", ".yhigh",
                         ".ylow", "latitude", "longitude", ".", ".value",
                         ".latitude", ".longitude", ".start_date",
                         ".end_date", "X", "Y", "checksum",
                         "..type", "ref", "value"))

                          # like to be able to drop a fe of the above...


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
# now replacing with dte_localArgsTests

dte_ggshellTestArgs <- function(.xargs, data){
  dte_localArgsTests(.xargs, data)
}

dte_localArgsTests <- function(.xargs, data){
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



dte_GeomArgs <- function(GP){
  #for GeomPoint, etc
  # not sure if this needs
  #    GP$optional_aes, GP$non_missing_aes as well?
  # like to have an option to add in stat args
  t1 <- c(GP$required_aes, names(GP$default_aes), GP$extra_params,
             "group")
  t1 <- unlist(strsplit(t1, "[|]"))
  unique(c(t1))
}


# could this go in test ???

dte_ggshellTidyArgs <- function(args, type=NULL){

    # tidy/rationalise args for dte_ggshell
    ###############################

    # check for type specific args
    #     handle [plot.type].[arg]s etc...
    args$..test <- "OK"
    if(!is.null(type)){
      args2 <- args[grepl(paste("^", type, "[.]", sep=""), names(args))]
      names(args2) <- gsub(paste(type, "[.]", sep=""), "", names(args2))
      args <- modifyList(args, args2)
      #########################
      # messy points=FALSE will slips through
      if(type %in% names(args)){
        if(is.logical(args[[type]]) && !all(args[[type]])){
          args$..test <- "not OK"
        }
      }
    }

    # col/color/colour handling... after assume colour...
    names(args)[names(args) %in% c("col", "color")] <- "colour"
    args <- args[!duplicated(names(args), fromLast=TRUE)]

    #out
    args
  }

# planning to replace above with this...

dte_localArgsTidies <- function(args, type=NULL,
                                tidy.name=NULL,
                                tidy.ref=NULL){

  # tidy/rationalise args for dte_ggshell
  ###############################

  # check for type specific args
  #     handle [plot.type].[arg]s etc...
  args$..test <- "OK"
  if(!is.null(type)){
    args2 <- args[grepl(paste("^", type, "[.]", sep=""), names(args))]
    names(args2) <- gsub(paste(type, "[.]", sep=""), "", names(args2))
    args <- modifyList(args, args2)
    #########################
    # messy points=FALSE will slips through
    if(type %in% names(args)){
      if(is.logical(args[[type]]) && !all(args[[type]])){
        args$..test <- "not OK"
      }
    }
  }

  # col/color/colour handling... after assume colour...
  # replace with a tidy list
  # to be tidied....
  if(is.null(tidy.name)){
    tidy.name <- list(c("col", "colour"),
                      c("color", "colour"))
  }
  if(!is.null(tidy.name)){
    #assume list
    for(i in tidy.name){
      names(args)[names(args) %in% i[1]] <- i[2]
    }
  }
  args <- args[!duplicated(names(args), fromLast=TRUE)]

  if(!is.null(tidy.ref)){
    #assume function
    #keep ..test
    .check <- c(names(formals(tidy.ref)), "..test")
    args <- args[names(args) %in% .check]
  }

  #out
  args
}

dte_ggshellFacet <- function(plt, data, x, y, args){

  #rebuild mapping for faceting
  plt$data <- data
  #plt$mapping$x <- data[[x]]
  #plt$mapping$x <- rlang::parse_quo(x, env=environment())
  #plt$mapping$y <- data[[y]]
  #plt$mapping$x <- rlang::parse_quo(y, env=environment())


  #don't need this if no facet plotting to worry about...
  if(!"facet.type" %in% names(args)){
    args$facet.type <- "wrap"
  } else {
    check <- c("wrap", "grid", "grid.col", "grid.row")
    if(!args$facet.type %in% check){
      warning("bad facet.type resetting to wrap")
      args$facet.type <- "wrap"
    }
  }
  #facet options
  if("facet" %in% names(args)){
    ## facet.type choices limited earlier
    #####################
    #tesing
    # passing just ggplot2::facet... args forward
    # names(formals(ggplot2::facet_grid)), etc....
    # if you give it facet, rows or cols
    #     these will override as well
    # does the job but code is messy...
    #####################
    ..xargs <- if(args$facet.type=="wrap"){
      args[names(args) %in% names(formals(ggplot2::facet_wrap))]
    } else {
      #must be a facet_grid
      args[names(args) %in% names(formals(ggplot2::facet_grid))]
    }
    if(length(args$facet)== 1){
      if(args$facet.type=="wrap"){
        ..xargs <- modifyList(list(facets=ggplot2::vars(.data[[args$facet[1]]])),
                              ..xargs)
        plt <- plt + do.call(ggplot2::facet_wrap, ..xargs)
        #plt <- plt + ggplot2::facet_wrap(facets=ggplot2::vars(.data[[.xargs$facet[1]]]))
      }
      if(args$facet.type %in% c("grid", "grid.row")){
        ..xargs <- modifyList(list(rows=ggplot2::vars(.data[[args$facet[1]]])),
                              ..xargs)
        plt <- plt + do.call(ggplot2::facet_grid, ..xargs)
        #plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(.data[[.xargs$facet[1]]]))
      }
      if(args$facet.type %in% c("grid.col")){
        ..xargs <- modifyList(list(cols=ggplot2::vars(.data[[args$facet[1]]])),
                              ..xargs)
        plt <- plt + do.call(ggplot2::facet_grid, ..xargs)
        #plt <- plt + ggplot2::facet_grid(cols=ggplot2::vars(.data[[.xargs$facet[1]]]))
      }
    } else{
      if(args$facet.type %in% c("wrap")){
        ..xargs <- modifyList(list(facets=c(ggplot2::vars(.data[[args$facet[1]]]),
                                            ggplot2::vars(.data[[args$facet[2]]]))),
                              ..xargs)
        plt <- plt + do.call(ggplot2::facet_wrap, ..xargs)
        #plt <- plt + ggplot2::facet_wrap(facets=c(ggplot2::vars(.data[[.xargs$facet[1]]]),
        #                                 ggplot2::vars(.data[[.xargs$facet[2]]])))
      }
      if(args$facet.type %in% c("grid", "grid.row")){
        plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(.data[[args$facet[1]]]),
                                         cols=ggplot2::vars(.data[[args$facet[2]]]))
      }
      if(args$facet.type %in% c("grid.col")){
        plt <- plt + ggplot2::facet_grid(cols=ggplot2::vars(.data[[args$facet[1]]]),
                                         rows=ggplot2::vars(.data[[args$facet[2]]]))
      }
    }
  }
  print("Hi")
  return(plt)
}







#################################
# dte_too.far
#################################

# maybe c++ version ???
#    see
#   https://gist.github.com/mikmart/fb191894f29af03f69d3a0705ef1ee48#file-matsums-c
# this work like exclude

## x<-rnorm(100);y<-rnorm(100) # some "data"
## n<-40 # generate a grid....
## mx<-seq(min(x),max(x),length=n)
## my<-seq(min(y),max(y),length=n)
## gx<-rep(mx,n);gy<-rep(my,rep(n,n))
## tf<-mgcv::exclude.too.far(gx,gy,x,y,0.1)
## plot(gx[!tf],gy[!tf],pch=".");points(x,y,col=2)
## tf1<-dte_too.far(data.frame(a=gx,b=gy),data.frame(a=x,b=y),0.1)
## plot(gx[!tf1],gy[!tf1],pch=".");points(x,y,col=2)


dte_too.far <- function(d1, d2, dist=0.1){
  # like mgcv::exclude.too.far but for n args
  # d1 are the cases to test
  #   assuming all named columns in d2 are to be tested
  # d2 is the reference frame
  #   assuming all names columns in d1 are also present in d2
  #   and everything else is to be ignored

  ## testing structures
  .test <- names(d1)
  if(!all(.test %in% names(d2))){
    stop("[dte_too.far] d2 does not contain all named elements of d1")
  }
  #align d2 with d1
  d2 <- d2[.test]

  ## rescale d1 and d2
  for(ii in names(d1)){
    .m1 <- min(d1[, ii], na.rm=TRUE)
    .m2 <- max(d1[, ii], na.rm=TRUE)
    d2[,ii] <- (d2[, ii]-.m1)/(.m2-.m1)
    d1[, ii] <- (d1[, ii]-.m1)/(.m2-.m1)
  }
  ans <- rep(NA, nrow(d1))
  temp <- d2
  sc <- paste(names(d1), collapse ="+")
  for(jj in 1:nrow(d1)){
    for(ii in names(d1)){
      temp[,ii] <- (d1[jj,ii] - d2[,ii])^2
    }
    #temp2[1:length(temp2)] <- sqrt(rowSums(temp))
    #ans[jj] <- all(temp2>dist)
    ##ans[jj] <- all(sqrt(rowSums(temp))>dist)
    ##ans[jj] <- all(sqrt(colSums(t(temp)))>dist)
    ## fastest so far...
    ans[jj] <- all(sqrt(with(temp,  eval(parse(text=sc))))>dist)
  }
  return(ans)
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




# trying to get facetting to work...

tubeMap.old <-
  function(data, x=NULL, y=NULL, ...){

    #setup
    ####################
    #args
    .xargs <- list(...)
    .xargs <- dte_ggshellTidyArgs(.xargs)
    #data
    if("ggplot" %in% class(data)){
      previous <- data
      d2 <- data$data
    } else {
      d2 <- data
      previous <- NULL
    }
    if("new.data" %in% names(.xargs)) {
      d2<-.xargs$new.data
    }

    if(is.null(x)){
      x <- ".longitude"
    }
    if(is.null(y)){
      y <- ".latitude"
    }
    .x <- getTubeX(d2, x, if.err="stop<<tubeMap>>x")
    .y <- getTubeX(d2, y, if.err="stop<<tubeMap>>y")

    #############################
    # without any this dies if length(names(.xargs)) > 1
    #############################
    # currently not facetting tubeMaps
    if(length(names(.xargs))>0 && any(grepl("^facet", names(.xargs)))){
      stop("[tubeMap] Sorry, map facet currently disabled",
           call. = FALSE)
    }
    ########################
    #
    if(!"grid.borders" %in% names(.xargs)){
      .xargs$grid.borders <- 0.05
    }

    ## build map layer
    ## (if not already a plot...)
    ###################
    if(is.null(previous)){
      #map ranges
      rng.lat <- range(.y, na.rm=TRUE)
      rng.lon <- range(.x, na.rm=TRUE)
      #adding border
      exp <- function(rng, n=0.1){
        temp <- (rng[2]-rng[1])*n
        c(rng[1]-temp, rng[2]+temp)
      }
      .gb <- .xargs$grid.borders
      rng.lon <- exp(rng.lon, .gb)
      rng.lat <- exp(rng.lat, .gb)
      dc <- OpenStreetMap::openmap(c(rng.lat[2], rng.lon[1]),
                                   c(rng.lat[1],rng.lon[2]),
                                   zoom = NULL,
                                   type =  "esri",
                                   mergeTiles = TRUE)
      dc <- suppressMessages(suppressWarnings(
        OpenStreetMap::openproj(dc,
                                projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ))
      map <- OpenStreetMap::autoplot.OpenStreetMap(dc)
      # general map text, formatting and sizing
      suppressMessages(suppressWarnings(
        map <- map + ggplot2::geom_text(data=data.frame(),
                                        ggplot2::aes(x=rng.lon[2], y=rng.lat[1]),
                                        label="Map layer: (c) OpenStreetMap/ESRI contributors    ",
                                        size=1.8, hjust=1, vjust=-0.5) +
          ggplot2::coord_quickmap() +
          ggplot2::theme_void()
      ))
      map$data <- d2 # cludge because autoplay not using my data

    } else {
      map <- previous
    }

    # add polygon
    if(any(grepl("^polygon", names(.xargs)))){
      ##########################
      #testing this tidy
      #    Holding code because it is very sensitive to ordering
      #.xargs2 <- .xargs[grepl("^polygon[.]", names(.xargs))]
      #names(.xargs2) <- gsub("polygon[.]", "", names(.xargs2))
      #names(.xargs2)[names(.xargs2) %in% c("col", "color")] <- "colour"
      #.xargs2 <- .xargs2[!duplicated(names(.xargs2), fromLast=TRUE)]
      #.xargs2 <- modifyList(.xargs, .xargs2)
      .xargs2 <- dte_ggshellTidyArgs(.xargs, "polygon")
      if(.xargs2$..test=="OK"){
        .xargs2 <- modifyList(list(x="X", y="Y"), .xargs2)
        ##########################
        # to think about...
        #    this currently needs polygon to be a sf polygon...
        #        BUT polygon could be a different object type ...
        #        OR polygon could be true... then you would use the data as polygon source
        #            can't colour a polygon by palette at moment
        .xargs2$polygon <- as.data.frame(sf::st_coordinates(.xargs2$polygon))
        drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPolygon)]
        map <- dte_ggshellAddGeom(.xargs2, .xargs2$polygon, map,
                                  ggplot2::geom_polygon,
                                  defaults = list(na.rm=TRUE, colour="blue",
                                                  fill="blue", alpha=0.25),
                                  drops = drops)
      }
    }

    # add surface
    ############################
    # this need tidying for contour and col handling
    # issue this error because col terms is fitted...
    # tubeMap(a, point=T, surface.fill=".value", too.far=0.1, grid.resolution=400, contour=T, point.col=".value", polygon=dont.share::caz.bradford, grid.borders=0.05, col=".value")
    #
    if(any(grepl("^surface", names(.xargs)))){
      #just arg should trigger this contour ??
      .xargs2 <- dte_ggshellTidyArgs(.xargs, "surface")
      .xargs2.test <- dte_ggshellTestArgs(.xargs2, d2)
      .test <- names(.xargs2.test[.xargs2.test=="data"])
      .test <- .test[.test %in% c("fill", "z", "colour", "contour")]
      if(length(.test)>0){
        .fit.args <- modifyList(list(data=d2, tube=.xargs2[[.test[1]]],
                                     inputs=c(x,y), by=c(.xargs$group, .xargs$group),
                                     simplify=TRUE, new.data="input.ranges"),
                                .xargs2[names(.xargs2) %in% c("too.far",
                                                              "grid.resolution",
                                                              "grid.borders")])
        .d2 <- do.call(fitTubeModel, .fit.args)
        .xargs2[[.test[1]]] <- paste(.xargs2[[.test[1]]], ".pred", sep="")
        if(.xargs2$..test=="OK"){
          .xargs2 <- modifyList(list(x=x, y=y), .xargs2)
          ##########################
          # if fill in there
          if("fill" %in% names(.xargs2)){
            .xargs2$fill <- .xargs2[[.test[1]]]
            drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomTile)]
            drops <- c(drops, "col", "color", "colour")
            map <- dte_ggshellAddGeom(.xargs2, .d2, map,
                                      ggplot2::geom_tile,
                                      defaults = list(na.rm=TRUE, alpha=0.5),
                                      drops = drops)
          }
          if("contour" %in% names(.xargs2)){
            ###################
            # fix for contour.[whatever]
            # maybe contour should be black if no fill and no col..?
            .xargs2$z <- .xargs2[[.test[1]]]
            drops <-  names(.xargs2)[!names(.xargs2) %in% c(dte_GeomArgs(ggplot2::GeomContour),
                                                            "z")]
            drops <- c(drops, "fill")
            map <- dte_ggshellAddGeom(.xargs2, .d2, map,
                                      ggplot2::geom_contour,
                                      defaults = list(na.rm=TRUE,
                                                      colour="white"),
                                      drops = drops)
            map <- dte_ggshellAddGeom(.xargs2, .d2, map,
                                      metR::geom_text_contour,
                                      defaults = list(na.rm=TRUE,
                                                      colour="white"),
                                      drops = drops)
          }

        }

      }

    }

    #add point(s)
    if(any(grepl("^point", names(.xargs)))){
      ##########################
      # testing this tidy
      .xargs2 <- dte_ggshellTidyArgs(.xargs, "point")
      if(.xargs2$..test=="OK"){
        .xargs2 <- modifyList(list(x=x, y=y), .xargs2)
        ##########################
        drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPoint)]
        map <- dte_ggshellAddGeom(.xargs2, d2, map,
                                  ggplot2::geom_point,
                                  defaults = list(na.rm=TRUE),
                                  drops = drops)
      }
    }

    ##################
    # to do
    #################
    # point mean, count, etc...
    # add faceting
    #     might be a big job ...
    #        the openstreetmap::auto.plot does not want to facet
    # add grouping ???
    # add/tidy structure - palette, legend, ect...

    #
    return(map)
  }

tubeMap.older <-
  function(data, x=NULL, y=NULL, previous=NULL, ...){

    #setup
    ####################
    d2 <- data
    .xargs <- list(...)
    #what to do about col/colour...?
    #   currently using this cludge in both tubePlot and ggplotTubeShell
    #      and assuming colour hereafter...
    names(.xargs)[names(.xargs) %in% c("col", "color")] <- "colour"
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]
    if(is.null(x)){
      x <- ".longitude"
    }
    if(is.null(y)){
      y <- ".latitude"
    }
    .x <- getTubeX(d2, x, if.err="stop<<tubeMap>>x")
    .y <- getTubeX(d2, y, if.err="stop<<tubeMap>>y")

    #check for facet
    if("facet" %in% names(args)){
      d2 <- checkTubeData(d2, x=.xargs$facet, n.x=2,
                          if.err = "stop<<ggplotTubeShell>>facet")
    }

    ##build map layer
    ###################
    if(is.null(previous)){
      #map ranges
      rng.lat <- range(.y, na.rm=TRUE)
      rng.lon <- range(.x, na.rm=TRUE)
      #adding border
      exp <- function(rng, n=0.1){
        temp <- (rng[2]-rng[1])*n
        c(rng[1]-temp, rng[2]+temp)
      }
      rng.lon <- exp(rng.lon, 0.05)
      rng.lat <- exp(rng.lat, 0.05)
      dc <- OpenStreetMap::openmap(c(rng.lat[2], rng.lon[1]),
                                   c(rng.lat[1],rng.lon[2]),
                                   zoom = NULL,
                                   type =  "esri",
                                   mergeTiles = TRUE)

      dc <- suppressMessages(suppressWarnings(
        OpenStreetMap::openproj(dc,
                                projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ))

      ###############################
      # modifying from OpenStreetMap
      # map <- OpenStreetMap::autoplot.OpenStreetMap(dc)

      p1 <- dc$bbox$p1
      p2 <- dc$bbox$p2
      map <- ggplot2::ggplot(d2, ggplot2::aes(x=.data[[x]],
                                                y=.data[[y]]))
#      map <- ggplot2::ggplot(data = d2)
     map <- map + ggplot2::expand_limits(x = c(p1[1], p2[1]),
                                        y = c(p2[2], p1[2])) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) + ggplot2::scale_y_continuous(expand = c(0, 0))

      for (tile in dc$tiles) {
        p1 <- tile$bbox$p1
        print(p1)
        p2 <- tile$bbox$p2
        print(p2)
        yres <- tile$yres
        print(yres)
        xres <- tile$xres
        print(xres)
        xseries <- seq(p1[2], p2[2], length=xres)
        yseries <- seq(p1[1], p2[1], length=yres)


        rast <- as.raster(matrix(tile$colorData, nrow = tile$xres, byrow = TRUE))
        annot <- ggplot2::annotation_raster(rast, p1[1] - 0.5 * abs(tile$bbox$p1[1] -
                                                  tile$bbox$p2[1])/yres, p2[1] + 0.5 * abs(tile$bbox$p1[1] -
                                                  tile$bbox$p2[1])/yres, p2[2] - 0.5 * abs(tile$bbox$p1[2] -
                                                  tile$bbox$p2[2])/xres, p1[2] + 0.5 * abs(tile$bbox$p1[2] -
                                                  tile$bbox$p2[2])/xres)

        map <- map + annot
      }
      #map + ggplot2::coord_equal()
      #################################
      # general map text, formatting and sizing
      suppressMessages(suppressWarnings(
        map <- map + ggplot2::geom_text(data=data.frame(),
                                        ggplot2::aes(x=rng.lon[2], y=rng.lat[1]),
                                        label="Map layer: (c) OpenStreetMap/ESRI contributors    ",
                                        size=1.8, hjust=1, vjust=-0.5) +
          ggplot2::coord_quickmap() +
          ggplot2::theme_void()
      ))
    } else {
      map <- previous
    }


    if(any(grepl("^polygon", names(.xargs)))){
      ##########################
      #tidy this
      #    ALSO very sensitive to ordering
      .xargs2 <- .xargs[grepl("^polygon[.]", names(.xargs))]
      names(.xargs2) <- gsub("polygon[.]", "", names(.xargs2))
      names(.xargs2)[names(.xargs2) %in% c("col", "color")] <- "colour"
      .xargs2 <- .xargs2[!duplicated(names(.xargs2), fromLast=TRUE)]
      .xargs2 <- modifyList(.xargs, .xargs2)
      .xargs2 <- modifyList(list(x="X", y="Y"), .xargs2)
      ##########################
      # to think about...
      #    this currently needs polygon to be a sf polygon...
      #        BUT polygon could be a different object type ...
      #        OR polygon could be true... then you would use the data as polygon source
      #            can't colour a polygon by palette at moment
      .xargs2$polygon <- as.data.frame(sf::st_coordinates(.xargs2$polygon))
      drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPolygon)]
      map <- dte_ggshellAddGeom(.xargs2, .xargs2$polygon, map,
                                ggplot2::geom_polygon,
                                defaults = list(na.rm=TRUE, colour="blue",
                                                fill="blue", alpha=0.25),
                                drops = drops)

    }

    if(any(grepl("^point", names(.xargs)))){
      ##########################
      #tidy this
      #    ALSO very sensitive to ordering
      .xargs2 <- .xargs[grepl("^point[.]", names(.xargs))]
      names(.xargs2) <- gsub("point[.]", "", names(.xargs2))
      names(.xargs2)[names(.xargs2) %in% c("col", "color")] <- "colour"
      .xargs2 <- .xargs2[!duplicated(names(.xargs2), fromLast=TRUE)]
      .xargs2 <- modifyList(.xargs, .xargs2)
      .xargs2 <- modifyList(list(x=x, y=y), .xargs2)
      ##########################
      drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPoint)]
      map <- dte_ggshellAddGeom(.xargs2, d2, map,
                                ggplot2::geom_point,
                                defaults = list(na.rm=TRUE),
                                drops = drops)

    }

    ##################
    # to do
    #################
    # point mean, count, etc...
    # add faceting
    # add grouping ???
    # add surface layer
    # add/tidy structure - palette, legend, ect...

    #
    return(map)
  }









