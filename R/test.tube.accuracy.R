############################################
#' @title Diffusion Tube Accuracy
############################################

#' @name test.tube.accuracy
#' @aliases test.tube.accuracy testTubeAccuracy
#' @description Testing diffusion tube accuracy. Coded methods are all
#' currently based on the comparison of aligned diffusion tube (DT) and
#' reference method (continuous analyser, CA) measurements.

# get aurn (or CA) data using openair or Defra
# find nearest DTs using AQEval findNearLatLon (or not)
# check near enough
# get for AURN data NO2 averages for sampling periods using AQEval::calcDateRangeStat()
# merge data
# plot and calculate stats

# find nearest tubes
#     get unique lat, lon and site and use as ref with findNearLatLon
#        and the AURN/ca site lat lon
# subset for just those tubes
# calculate the sample ranges needed
#      as standard months
#      as in defra calendar
# merge this and dts
# plot and calculate stats

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of conventionally formatted DT records.
#' @param data.ref Reference data source, typically a data.frame or similar,
#' containing data-series of conventionally formatted CA
#' records not already aggregated, time aligned or added to \code{data}.
#' @param tube The name of the data-series (in \code{data}) to test, typically
#' DT NO2 concentrations (in ug/m3).
#' @param ref The name of the data-series (in \code{data.ref}) to compare with \code{tube}.
#' @param method The method to apply when aggregating \code{ref} (and
#' \code{data.ref}) to compare with \code{tube} (and \code{data}). There is
#' currently one method, set up to run with \code{openair}-format
#' \code{data.ref} data sets. This method aggregates and averages
#' \code{data.ref} at the sampling resolution of the tube data set, \code{data}, as
#' identified from tags built using \code{\link{tagTube}} methods, before merging
#' tube and reference data sets. See below or ?\code{\link{tagTube}}
#' for further details.
#' @param max.distance the maximum distance (m) between tube and reference
#' method locations when pairing diffusion tube and reference measurements for
#' comparison.
#' @param show The outputs to show when returning results, by default the
#' plot and summary report.
#' @param ... Additional arguments, currently passed to
#' \code{\link{tagTube}} to check tube tags, and \code{\link{ggplotTubeShell}}
#' to handle generic plotting arguments.

#' @return \code{testTubeAccuracy} returns a list of test results, typically
#' \code{data}, the match (tube and reference) measurement pairs less than
#' \code{max.distance} apart based on tagged sampling dates and locations,
#' \code{all}, all date-based measurement pairs regardless of distance apart,
#' \code{plot}, and results \code{lookup} table and \code{report}.
#'
#' @details Both sampling dates and locations are identified/tagged using
#' \code{\link{tagTube}} methods. \code{\link{tagTubeDates}} tags sampling
#' dates and \code{\link{tagTubeLatLon}} tags locations.
#'
#' NOTE: \code{testTubeAccuracy} attempts to tag \code{data} using default
#' \code{\link{tagTube}} setting if it has not already been tagged, but it
#' is probably better to tag and check all data yourself before running more
#' detailed analysis.
#'
#' \code{testTubeAccuracy} also uses \code{\link{ggplotTubeShell}} to handle
#' common plotting arguments, e.g. X and Y axes label handling. Plot
#' conditioning arguments like \code{group} and \code{facet} are also
#' tracked by \code{testTubeAccuracy} itself and used to subset data
#' when comparing \code{tube} and near-\code{ref} measurements.
#'
#' When merging \code{data} and \code{data.ref}, \code{testTubeAccuracy} adds
#' a \code{'.ref'} suffix to data columns from \code{data.ref}, so it you want
#' to facet or group data by a column in \code{data.ref}, you need to include
#' the suffix when naming it.

# see bodge 3/3...
#    need this is data.ref names in output are sometimes name and sometimes
#         name.ref depending on if name in both data and data.ref...

## notes

#source code  site     site_type latitude longitude variable Parameter_name start_date
#<chr>  <chr> <chr>    <chr>        <dbl>     <dbl> <chr>    <chr>          <dttm>
#  1 aurn   BHA4  Bath A4… Urban Tr…     51.4     -2.36 NO2      Nitrogen diox… 2019-10-24 00:00:00
#2 aurn   BATH  Bath Ro… Urban Tr…     51.4     -2.35 NO2      Nitrogen diox… 1996-11-18 00:00:00
#3 NA     NA    NA       NA            NA       NA    NA       NA             NA
# ℹ 5 more variables: end_date <chr>, ratified_to <dttm>, zone <chr>, agglomeration <chr>,
#   local_authority <chr>

# looks like BHA4 (2019 onwards) and BATH (up until oct-2019) are my local AURN...
# data.ref <- openair::importAURN("bha4", year=2019:2024, meta=TRUE)
# testTubeAccuracy(dont.share::dt.banes, data.ref, tube="measurement", ref="no", facet="year_of_measurement")


## notes

#> subset(openair::importMeta(all=TRUE), variable=="NO2" & local_authority=="Leeds")
## A tibble: 2 × 14
#source code  site     site_type latitude longitude variable Parameter_name start_date
#<chr>  <chr> <chr>    <chr>        <dbl>     <dbl> <chr>    <chr>          <dttm>
#  1 aurn   LEED  Leeds C… Urban Ba…     53.8     -1.55 NO2      Nitrogen diox… 1993-01-04 00:00:00
#2 aurn   LED6  Leeds H… Urban Tr…     53.8     -1.58 NO2      Nitrogen diox… 2008-02-17 00:00:00
## ℹ 5 more variables: end_date <chr>, ratified_to <dttm>, zone <chr>, agglomeration <chr>,
##   local_authority <chr>

# data.ref <- openair::importAURN(c("LEED", "LED6"), year=2019:2024, meta=TRUE)
# testTubeAccuracy(dont.share::dt.leeds, data.ref, tube="measurement", ref="no", facet="year_of_measurement")


#' @rdname test.tube.accuracy
#' @export
testTubeAccuracy <-
  function(data, data.ref = NULL,
           tube = "measurement", ref = "no2",
           method = 1, max.distance = 10,
           show = c("plot", "report"), ...){

    #thinking about
    #######################

    #   simplifying group and facet handling
    #      use checkTubeData like in testTubePrecision

    #  can we build the data.frame data/ref.data dataset then
    #      do the group/facet/.cut work, these can use terms in ref...?
    #      thin about renaming data in ref.data ref.[name]s ?
    #          (site in both...)

    #  some AQEval issues to look at...
    #      noted in code below

    #   how to handle replicates, e.g. triplicates
    #      compare as three same-ref points (current)
    #      or average them and compare the averages


    #   do we want option to handle data with NO2 already aggregated and merged
    #      ref is NULL and y is in data, then we don't need to aggregate
    #          feels a bit minimal...

    #set up
    ###############################

    #chasing the dots
    .xargs <- list(...)

    #tagTube check/add tags
    data <- tagTube(data, ...)

    #tube handling
    # error if not numeric via getTUbeX
    data$.tube <- getTubeX(data, tube,
                           test.class = "numeric",
                           if.err="stop<<testTubeAccuracy>>tube")


    #ref handling
    if(!ref %in% names(data.ref)){
      stop("[testTubeAccuracy]> Expecting '", ref, "' in supplied data.ref \n",
           call.=FALSE)
    }
    # think about also checking data if data.ref is not supplied ???
    # or could do this with getTubeX and add here ???
    #    but not current as .ref because I a use that elsewhere

#################
# moved group/facet/.cut args from here
#################

##############
#start of new code to allow multiple CAs
    #working but needs work...
##########

    #calc distance from tube to ref site
    #####################################
    # will now work with multiple sites in data.ref...
    # also AQEval wants its data lat/lon named latitude and longitude
    # also it reorders by distance (and I want type in order in data...)

    # there are tube lat/longs plus a counter...
    # renamed to be matched against ref.data
    t.ll <- data[c(".latitude", ".longitude")]
    t.ll$cheat <- 1:nrow(t.ll)
    names(t.ll) <- gsub("[.]", "", names(t.ll))
    # all the unique
    rf.test <- unique(paste(data.ref$latitude, data.ref$longitude, sep="<>"))

    #repeat for each unique ref lat/lon
    #####################################
    out <- lapply(rf.test, function(i){
      .ref <- subset(data.ref, paste(latitude, longitude, sep="<>") == i)
      #########################
      #bodge 1/3
      #holding on to these to fix out after AQEval::calcDateRangeStat
      .rr <- .ref[1,]
      #########################
      temp <- AQEval::findNearLatLon(.ref$latitude, .ref$longitude,
                                     ref=t.ll, nmax=nrow(t.ll))
      .data <- data #map onto all data
      .data$distance.m <- temp$distance.m[order(temp$cheat)]
      #aggregate ref .data and make it tube-like
      #########################################
      temp <- .data[!duplicated(paste(.data$.start_date, .data$.end_date)),]
      .ref <- AQEval::calcDateRangeStat(.ref, from=temp$.start_date,
                                        stat = .xargs[["stat"]],
                                            to=temp$.end_date, method=method)
##################
      #speed testing this is the method set in call at moment
##################
      #sending own stat include a 'if not numeric return x[1]' has issue!!!
      # making everything character for faster method 2 but OK from method 1
      # NOTED as issue, see AQEval notes...
      names(.ref)[1:2] <- c(".start_date", ".end_date")
      ####################################
      #bodge 2/3
      #  getting source, site and code from .rr
      #  might not have these if users has non-openair dataset....
      .ref[c("source", "site", "code")] <- .rr[c("source", "site", "code")]
      ####################################
      #bodge 3/3
      # bit messy but leaving for now but...
      #    I would column names from ref to be consistent in output
      #    the left_join add suffix only when names are in both and not in by...
      #    this just makes from ref all [name].ref
      #          (would get messy if similar already in .ref)
      #    personally, would have preferred ref.[name] rather than [name].ref...
      #           but not worrying for now...
      names(.ref) <- ifelse(names(.ref) %in% c(".start_date", ".end_date"),
                            names(.ref), paste(names(.ref), ".ref", sep=""))
      #  shouldn't need the suffixes but leaving just in case...
      out <- dplyr::left_join(.data, .ref, by=c(".start_date", ".end_date"),
                              suffix = c("", ".ref"))
      #merge data
      out$.tube <- out[,tube]
      out$.ref <- out[,paste(ref, ".ref", sep="")]
      out
    })
    out <- do.call(rbind, out)
    #bodge 3/3... last bit
    ref <- paste(ref, ".ref", sep="")

##################
# end of distance calc AND merge
##################

    # don't think there is a point to keeping unpaired cases
    #   plus it generates a ggplot warning for missing cases
    #   BUT maybe think about this
    out <- out[!is.na(out$.tube) & !is.na(out$.ref),]

    if(is.null(out) || nrow(out)<1){
      stop("[testTubeAccuracy]> Halting test; no tube/data.ref pairs (check sources?)",
           call.=FALSE)
    }


###################
# testing doing group and facet here...
#      switch data to out...
###################

    #group and facet
    #tracking for data cutting (calling .cut)
    group <- .xargs[["group"]]
    #using .xargs[["whatever"]] because .xargs$whatever partial matches...
    if(length(group)>1){
      stop("[testTubeAccuracy]> Sorry, only one group term allowed \n",
           call.=FALSE)
    }
    facet <- .xargs[["facet"]]
    if(length(facet)>2){
      stop("[testTubeAccuracy]> Sorry, no more than two facet terms allowed \n",
           call.=FALSE)
    }
    if(length(group)>0){
      out[, group] <- getTubeX(out, group,
                                if.err = "stop<<testTubeAccuracy>>group")
    }
    if(length(facet)>0){
      for(i in 1:length(facet)){
        out[, facet[i]] <- getTubeX(out, facet[i],
                                     if.err = "stop<<testTubeAccuracy>>facet")
      }
    }
    if(length(c(group, facet))==0){
      out$.cut <- "|all|"
    } else {
      out$.cut <- ""
      if(length(group)>0){
        out$.cut <- paste(out$.cut, out[, group[1]], sep="|")
      }
      if(length(facet)>0){
        out$.cut <- paste(out$.cut, out[, facet[1]], sep="|")
      }
      if(length(facet)>1){
        out$.cut <- paste(out$.cut, out[, facet[2]], sep="|")
      }
      out$.cut <- paste(out$.cut, "|", sep="")
    }

##########################
# end of group reposition
#########################

    #do by .cut
    ###########################
    # note currently doing this after dt and ref matched
    ls <- lapply(unique(out$.cut), function(z){
        test <- out[out$.cut==z,]
        local <- test[test$distance.m < max.distance,]
        if(nrow(local)>0){
          local <- local[!is.na(local$.tube) & !is.na(local$.ref),]
        }
        if(nrow(local)>3){
            form <- as.formula(paste(tube, ref, sep="~"))
            mod <- summary(lm(form, data=local))
            lookup <- data.frame(.cut=z,
                               n=nrow(local),
                               max.distance=max.distance,
                               adj.r.squared=mod$adj.r.squared,
                               intercept=mod$coefficients[1,1],
                               slope=mod$coefficients[2,1],
                               p.intercept=mod$coefficients[1,4],
                               p.slope=mod$coefficients[2,4])
            rep <- paste("  ", z, " ",
                         signif(mod$coefficients[1,1], 4), "\t+ ",
                         signif(mod$coefficients[2,1], 4), "*[ref]\t(adj.r^2 ",
                         signif(mod$adj.r.squared, 4), ")",
                         sep="", collapse="")
        } else {
          lookup <- data.frame(.cut=z,
                               max.distance=max.distance,
                               n=nrow(local),
                               adj.r.squared=NA,
                               intercept=NA,
                               slope=NA,
                               p.intercept=NA,
                               p.slope=NA)
          rep <- paste("  ", z, " Insufficient data...",
                       sep="", collapse="")
        }
        list(data=test, local=local, report=rep, lookup=lookup)
    })

    #repack lists
    test <- lapply(ls, function(x){
      x$data
    })
    test <- do.call(rbind, test)
    local <- lapply(ls, function(x){
      x$local
    })
    local <- do.call(rbind, local)
    lookup <- lapply(ls, function(x){
      x$lookup
    })
    lookup <- do.call(rbind, lookup)
    rep <- lapply(ls, function(x){
      x$report
    })
    rep <- paste(unlist(rep), collapse="\n")
    rep <- paste("'", tube, "' vs. '", ref,
                 "' (dist < ", max.distance, "):", "\n",
                 rep, sep="", collapse="")

    ## plot
    ######################################
    #tidy plot output
    if(nrow(local)<3){
      #not enough to plot...
      plt <- NULL
    } else {
      plt <- ggplotTubeShell(local, x=".ref", y=".tube",
                             xlab=ref, ylab=tube, ...) +
                ggplot2::geom_point() +
        # if we add col to plot, it does lm as well as colors...
        # so disconnects seen verses calculated...
        # to disconnect color from smooth in plot maybe ???
        # ggplot2::geom_smooth(method="lm", formula="y~x", ggplot2::aes(col=NULL))
                ggplot2::geom_smooth(method="lm", formula="y~x")
    }

    #show
    ##############################
    if("plot" %in% tolower(show)){
      if(!is.null(plt)){
        plot(plt)
      }
    }
    if("report" %in% tolower(show)){
      message(rep)
    }

    ## output
    #############################
    # do we also need to output  max.distance?
    out <- list(data=local, all=test, plot=plt, lookup=lookup, report=rep)
    return(invisible(out))
  }
















