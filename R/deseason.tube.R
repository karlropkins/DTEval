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
#' to (see \code{model}), typically the DT NO2 concentrations (in ug/m3).
#' @param by The name(s) of hierarchical grouping terms. These are used to
#' sub-sample the data when deseasonalising data.
#' @param ... additional arguments, currently ignored.

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
#    dd <- tagTube(dont.share::dt.bradford.2); dd <- dd[dd$.longitude<0,]
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

deseasonTubeData <- function(data, tube=".value", by=NULL, ...){

  # setup
  data <- tagTube(data)
  data <- tagTubeDate(data)
  .by <- c(".date", by)
  .d <- calcTubeStat(data, tube, by=.by)
  .d$jd <- as.numeric(format(.d$.date, "%j"))
  .d$n <- as.numeric(.d$.date)
  .d$.y<- .d[[paste(tube, ".mean", sep="")]]

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
  ans
}
