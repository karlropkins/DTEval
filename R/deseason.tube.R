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
#' @param method The deseasonalisation method to apply: 1 (default)
#' LOESS; 2 local LOESS, like \code{1} but LOESS is applied to
#' groups of one or more near sites, see also \code{Details}.
#' @param ... additional arguments, currently passed to \code{loess}.

#' @details
#' \code{deseasonTubeData} attempts to deseasonalise the supplied time-series.
#'
#' It builds a LOESS time-series model in the form:
#'
#' \code{[tube] ~ loess([day-of-year] + [date])}
#'
#' And, extracts the seasonal component as the response term for the day-of-year.
#'
#' \code{method 1} applies the LOESS model directly at the requested \code{by} level.
#'
#' \code{method 2} applies the LOESS model by-location in addition to the requested
#' \code{by} level. Used directly it is equivalent to:
#'
#' \code{deseasonTubeData(..., method=1, by=".location")}
#'
#' ... or similar, but can to expanded to build near-location models
#' using \code{max.distance} and/or \code{max.n} to set the maximum distance
#' between and/or number of sampling sites in the subset near data used to
#' build these models.
#'
#' @note \code{deseasonTubeData} and related functions assume that \code{data}
#' is a data set \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return the supplied \code{data.frame}
#' with attached predicted time-series components: \code{..season},
#' \code{..deseason}, etc.

#' @examples
#' # ambient data exhibits seasonality
#' tubePlot(dt.brd, ".date",".value", plot.type="smooth")
#' # underlying trend
#' mod<-deseasonTubeData(dt.brd,".value", by=".location")
#' tubePlot(mod,".date", "..trend", plot.type="smooth")

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
#    tubePlot(ans, x=".date", "..trend", col=".in_polygon", plot.type="smooth")

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

  # main setup
  .xargs <- list(...)

  # check methods
  .method.check=1:2
  if(!method %in% .method.check){
    stop("[deseasonTubeData] unknown method; maybe try ",
         paste(.method.check, collapse=","), "?",
         call.=FALSE)
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
      ########################
      # stop it building if d2 rows < 2
      # (testing)
      ########################
      if(nrow(d2)>2){
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
      ########################
      # close
      # (stop it building if d2 rows < 2)
      ########################
      } else {
        NULL
      }
    })
    ans <- do.call(rbind, ans)
  }

  # method 2 in development
  # this might replace method 1 ???
  if(method==2){

    # setup
    .by <- unique(c(".date", ".longitude", ".latitude", ".location", by))
    data <- tagTubeRequired(data, required = c(tube, .by), ...)

    # max.n / max.distance handling
    ####################################
    # could set max.distance to zero if
    #    neither max.distance or max.n are set ???
    # does this need a row < 2 check like method 1?
    ####################################
    if(!"max.n" %in% names(.xargs) & !"max.distance" %in% names(.xargs)){
       .xargs$max.distance <- 0
    }

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
      # should all outputs be associated with their current locations ONLY ??
      #    Currently JUST keeping the outputs for the modelled case...
      #       (at end of lapply)
      #    could also weight by distance in loess ???
      #       but needs thinking about because nearest sites are 0 distance away
      #    maybe think about reinstating span reset or surface ="direct" for loess
      #       (span dropped because to missing data issue...)
      ..test. <- AQEval::findNearLatLon(lat=.d[.d$..id==i,]$.latitude[1],
                           lon=.d[.d$..id==i,]$.longitude[1],
                           ref=data, nmax=nrow(data),
                           rename.ref.lat = ".latitude",
                           rename.ref.lon = ".longitude")
      # get the near sites (max.distance)
      # see note at start of method...?
      if("max.distance" %in% names(.xargs)){
        ..test. <- ..test.[..test.$distance.m <= .xargs$max.distance,]
      }
      ..test. <- unique(paste(..test.$.latitude, ..test.$.longitude))
      # get the near sites (max.nu)
      if("max.n" %in% names(.xargs)){
        if(length(..test.) > .xargs$max.n){
           ..test. <- ..test.[1:.xargs$max.n]
        }
      }
      # check max.distance/n response
      ## print(length(..test.))
      d2 <- .d[.d$..id %in% ..test., ]
      ########################
      # stop it building if d2 rows < 2
      # (testing - like with method 1 BUT
      #            later because max.distance
      #            and max.n can remove rows...)
      ########################
      if(nrow(d2)>2){

        row.names(d2) <- 1:nrow(d2)
        ##################
        # do we need to protect this from invalid passes ???
        # also do we need to
        #     reinstate span ??
        #     add surface = "direct" default ??
        #     (can be messy data...)
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
          d2$..trend <- d2$..trend - mean(d2$..trend)    # why no na.rm=TRUE??
          d2$..trend <- d2$..trend + mean(d2$..fit)      # (again, maybe check elsewhere)

          d2$..season <- d2$..fit - d2$..trend
          d2$..deseason <- d2$.y- d2$..season

          d2 <- d2[order(d2$.date), ]
          ##############################
          # see notes at start of lapply...
          #   how do we handle output
          #       currently just the main site/sample
          #       no matter the group size used...
          d2 <- d2[d2$..id==i,]
          d2
        }
      } else {
        NULL
      }
    })
    ans <- do.call(rbind, ans)

  }

  #output
  ans
}



