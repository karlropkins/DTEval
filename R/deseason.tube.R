#######################################################
#' @title Deseasonalise Diffusion Tube Data
#######################################################

#' @name deseason.tube
#' @aliases deseason.tube deseasonTubeData
#' @description Functions for deseasonalising multiple year
#' diffusion tube (DT) data time-series.

#  see notes on value of deseasonalisation

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param tube The name of the data-series (in \code{data}) to fit a model
#' to (see \code{method} and \code{Details}), typically the DT NO2
#' concentrations (in ug/m3).
#' @param by The name(s) of hierarchical grouping terms. These are used to
#' sub-sample the data when deseasonalising data.
#' @param method The deseasonalisation method to apply: 1 (default) LOESS.
#' @param ... additional arguments, currently passed to loess.

#' @details
#' \code{deseasonTubeData} attempts to deseasonalise the supplied time-series.
#'
#' It builds a LOESS time-series model in the form:
#'
#' \code{[tube] ~ loess([day-of-year] + [date])}
#'
#' And, extracts the seasonal component as response term for the day-of-year.
#'
#' @note \code{deseasonTubeData} and related functions assume that \code{data}
#' is a data set \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return the supplied \code{data.frame}
#' with attached predicted time-series components: \code{..season},
#' \code{..deseason}, etc.

# document stl guidance on this...
# it is not just a regular pattern you put from the data

#############################
# deseasonaliseTube
#############################

# need to document

# current test
#    dd <- dont.share::dt.bradford.2; dd <- dd[dd$.longitude<0,]
#    dd <- tubeInXYPolygon(dd, dont.share::caz.bradford)
#    ans <- deseasonTubeData(dd, "bias_adjusted_measurement", by=c(".in_polygon"))
#    tubePlot.old(ans, x=".date", "..deseason", col=".in_polygon", plot.type="smooth")

# TO DECIDE

# currently does
###############################
#     basic deseasonalisation for reports

#  thinking about
##############################

#   se reporting/handling
#   model performance
#   suspect models - there will be a lot given the sparsity of the data
#                    with some of the modelling strategies

# example????
##############################


#' @rdname deseason.tube
#' @export

deseasonTubeData <- function(data, tube=".value", by=NULL,
                             method=1, ...){

  # methods
  .method.check=1:2
  if(!method %in% .method.check){
    stop("[deseasonTubeData] unknown method; maybe try ",
         paste(.method.check, collapse=","), "?")
  }

  if(method==1){

    # loess model
    #    not documented but it currently accepts loess arguments via ...

    # setup
    .by <- c(".date", by)
    data <- tagTubeRequired(data, required = c(tube, .by), ...)

    # simplify...
    # option to turn this calcTubeStat ???
    #    (I think it would kill the loess)
    .d <- calcTubeStat(data, tube, by=.by)

    # numeric model inputs
    .d$jd <- as.numeric(format(.d$.date, "%j"))
    .d$n <- as.numeric(.d$.date)
    .d$.y<- .d[[paste(tube, ".mean", sep="")]]

    #index
    if(is.null(by)){
      .d$..id <- "|default|"
    } else {
      .tst <- paste("paste(", paste(by, sep=",", collapse = ","),
                    ", sep='')", sep="")
      .d$..id <- getTubeX(.d, .tst)
    }
    .id <- unique(.d$..id)

    ans <- lapply(.id, function(x){
      d2 <- .d[.d$..id==x,]
      row.names(d2) <- 1:nrow(d2)
      suppressWarnings(suppressWarnings(
        mod <- try(loess(.y ~ jd + n, data=d2,...),
                   silent = TRUE)
      ))
      if(inherits(mod, "try-error")){
        NULL
      } else {
        suppressWarnings(suppressWarnings(
          temp <- predict(mod, se=TRUE)
        ))
        d2$..fit <- NA
        d2$..fit[as.numeric(names(temp$fit))] <- temp$fit
        d2$..fit.se <- NA
        d2$..fit.se[as.numeric(names(temp$se.fit))] <- temp$se.fit
        temp <- d2
        temp$jd <- mean(temp$jd)
        suppressWarnings(suppressWarnings(
          temp <- predict(mod, temp, se=TRUE)
        ))
        d2$..trend <- NA
        d2$..trend[as.numeric(names(temp$fit))] <- temp$fit
        d2$..trend.se <- NA
        d2$..trend.se[as.numeric(names(temp$se.fit))] <- temp$se.fit
        d2$..trend <- d2$..trend - mean(d2$..trend)
        d2$..trend <- d2$..trend + mean(d2$..fit)

        d2$..season <- d2$..fit - d2$..trend
        d2$..deseason <- d2$.y- d2$..season

        d2 <- d2[order(d2$.date),]
        d2
      }
    })
    ans <- do.call(rbind, ans)
  }

  # method 2 in development
  # this might replace method 1 ???
  if(method==2){

    # setup
    .by <- c(".date", ".longitude", ".latitude", by)
    data <- tagTubeRequired(data, required = c(tube, .by), ...)

    # simplify...
    # option to turn this calcTubeStat ???
    #    (I think it would kill the loess)
    .d <- calcTubeStat(data, tube, by=.by)

    # numeric model inputs
    .d$jd <- as.numeric(format(.d$.date, "%j"))
    .d$n <- as.numeric(.d$.date)
    .d$.y<- .d[[paste(tube, ".mean", sep="")]]

    .d$..id <- getTubeX(.d, "paste(.latitude, .longitude)")
    # for each location...
    # get
    ans <- lapply(sort(unique(.d$..id)), function(i){
      ############################
      # maybe distance check other way around?
      # access distance.m in call (currently using as near.than)...
      # have near.than BUT also want may nearest n locations
      # should these be associated with their current locations ??
      #    OR should just the outputs for the modelled case be kept ??
      # could also weight by distance in loess ???
      # maybe think about setting span or surface ="direct" for loess
      ..test. <- AQEval::findNearLatLon(lat=.d[.d$..id==i,]$.latitude[1],
                           lon=.d[.d$..id==i,]$.longitude[1],
                           ref=data, nmax=nrow(data),
                           rename.ref.lat = ".latitude",
                           rename.ref.lon = ".longitude")
      # get the near sites (current hard coded...)
      ..test. <- ..test.[..test.$distance.m < 10,]
      ..test. <- unique(paste(..test.$.latitude, ..test.$.longitude))
      #check how many 'case' location + by... sampled
      #print(length(..test.))
      d2 <- .d[.d$..id %in% ..test., ]
      row.names(d2) <- 1:nrow(d2)
      suppressWarnings(suppressWarnings(
        mod <- try(loess(.y ~ jd + n, data=d2,...),
                   silent = TRUE)
      ))
      if(inherits(mod, "try-error")){
        NULL
      } else {
        suppressWarnings(suppressWarnings(
          temp <- predict(mod, se=TRUE)
        ))
        d2$..fit <- NA
        d2$..fit[as.numeric(names(temp$fit))] <- temp$fit
        d2$..fit.se <- NA
        d2$..fit.se[as.numeric(names(temp$se.fit))] <- temp$se.fit
        temp <- d2
        temp$jd <- mean(temp$jd)
        suppressWarnings(suppressWarnings(
          temp <- predict(mod, temp, se=TRUE)
        ))
        d2$..trend <- NA
        d2$..trend[as.numeric(names(temp$fit))] <- temp$fit
        d2$..trend.se <- NA
        d2$..trend.se[as.numeric(names(temp$se.fit))] <- temp$se.fit
        d2$..trend <- d2$..trend - mean(d2$..trend)
        d2$..trend <- d2$..trend + mean(d2$..fit)

        d2$..season <- d2$..fit - d2$..trend
        d2$..deseason <- d2$.y- d2$..season

        d2 <- d2[order(d2$.date),]
        d2
      }
    })
    ans <- do.call(rbind, ans)

  }

  #output
  ans
}



