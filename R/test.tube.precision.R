############################################
#' @title Diffusion Tube Precision
############################################

#' @name test.tube.precision
#' @aliases test.tube.precision testTubePrecision
#' @description Testing diffusion tube precision. Coded methods are all
#' currently based on the comparison of co-located replicate diffusion
#' tube (DT) samples.

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of conventionally formatted diffusion tube records.
#' @param tube The name of the data-series (in \code{data}) to test, typically
#' DT NO2 concentrations (in ug/m3).
#' @param n The test compares replicate samples collected from the same
#' location at the same time. \code{n} sets the number of replicates required,
#' by default 3 for triplicate measurements.
#' @param method The method to apply when testing \code{tube} data. See relevant
#' details below for further information.
#' @param show The outputs to show when returning results, by default the
#' plot and summary report.
#' @param ... Additional arguments, currently passed to
#' \code{\link{tagTube}} to check tube tags, and \code{\link{tubePlot}}
#' to handle generic plotting arguments.

#' @details \code{testTubePrecision} uses \code{\link{tagTube}}
#' to identify replicate samples, if not already tagged, then applies
#' one of the following methods to estimate the precision of cases
#' with \code{n} replicates:
#'
#' * \code{method = 1} uses \code{quantile(x, 0.05, 0.95)}
#'
#' * \code{method = 2} uses \code{mean(x) +/- qt(0.05/2, n(x)-1), lower.tail=FALSE)} &ast;
#' \code{sd(x)/sqrt(n(x))}
#'
#' * \code{method = 3} used \code{mean(x) +/- 1.96} &ast; \code{sd(x)/sqrt(n(x)))}
#'
#' * \code{method = 4} used \code{mean(x) +/- qnorm(0.975)} &ast; \code{sd(x)}

# document these properly when other methods go in...

#'
#' NOTE: \code{testTubePrecision} attempts to tag \code{data} using default
#' \code{\link{tagTube}} setting if it has not already been tagged, but it
#' is probably better to tag and check all data yourself before running more
#' detailed analysis.
#'
#' \code{testTubePrecision} also uses \code{\link{tubePlot}} handles
#' common plotting arguments, e.g. X and Y axes label handling. Plot
#' conditioning arguments like \code{group} and \code{facet} are also
#' tracked by \code{testTubePrecision} itself and used to subset data
#' when testing precision.


# NEED to document these and add UK and EURO methods

#' @return \code{testTubePrecision} returns a list of test results,
#' typically \code{data} all \code{n}-replicate measurement sets identified
#' using \code{\link{tagTube}}, \code{plot}, results \code{lookup} table
#' and; the (full) \code{report}.

#splatted function

########################
# removing full imports
## #' @import dplyr
## #' @import ggplot2
# now specifying directly in code, ggplot2::ggplot, etc

####################
# to think about
####################

## testTubePrecision(portsmouth, method=2)

## bias_adjusted_measurement' (rep = 3 subset):
##  [all] Insufficient replicates...

# runs without warning or error but generates nothing usable



#####################
# doing
#####################

#

#############################
# to do
#############################



# catching tube not numeric, but get error if year (a numeric)
#    sent as tube term
#    testTubePrecision(dt.york, tube="CalendarYear")
#    Error in data.frame(.cut = test$.cut[1], ans = temp) :
#     arguments imply differing number of rows: 1, 0
#        (not sure if we should be catching this???)

# think about facet/group/.cut handling
#    testTubePrecision/Accurcay

# tidy plot control
#    facet args currently:
#         facet.type = "wrap", "grid", "grid.row", "grid.col"
#         and any args passed forward via ...
#    group
#        force group/facet to factor ??
#             is just "factor(what.ever)" enough ??
#    control cols??
#    quicktext
#        added like AQEval but only x and y labels
#             and only when auto.text = TRUE in call...


# tidy report
#    is message best way ???

# better way of building the models?
#     different ways of doing methods
#     think about order of these ???
#          if change these, also need to update the documents
#     any other methods or alternative methods ???

# better document
#     methods need documenting and references included ???

# do we need to a count of how many sets of n replicates we have ??
#     (I think it is just nrow(.out$data) but could calculate in
#      lookup so it is by .cut and n=[not enough] to no data output)



#' @rdname test.tube.precision
#' @export


testTubePrecision <-
  function(data,
           tube = ".value",
           n = 3,
           method = 2,
           show = c("plot", "summary.report"),
           ...){

    #setup
    ########################

    #chasing the dots
    .xargs <- modifyList(list(auto.text=TRUE,
                              xlab="replicate mean",
                              ylab=tube), list(...))

    #tagTube, check/tag
    data <- tagTubeRequired(tagTube(data, ...), required=c(tube, unlist(.xargs)), ...)

    #tube handling
    # error if not numeric via getTubeX
    data$.tube <- getTubeX(data, tube,
                           test.class = "numeric",
                           if.err = "stop<<testTubePrecision>>tube")

    #group and facet
    # and data cutting
    group <- .xargs[["group"]]
    if(length(group)>1){
      stop("[testTubePrecision]> Sorry, only one group term allowed \n",
           call.=FALSE)
    }
    facet <- .xargs[["facet"]]
    if(length(facet)>2){
      stop("[testTubePrecision]> Sorry, no more than two facet terms allowed \n",
           call.=FALSE)
    }
    if(length(group)>0){
      data[, group] <- getTubeX(data, group,
                                if.err = "stop<<testTubePrecision>>group")
    }
    if(length(facet)>0){
      for(i in 1:length(facet)){
        data[, facet[i]] <- getTubeX(data, facet[i],
                                     if.err = "stop<<testTubePrecision>>facet")
      }
    }
    if(length(c(group, facet))==0){
      data$.cut <- "|all|"
    } else {
      data$.cut <- ""
      if(length(group)>0){
        data$.cut <- paste(data$.cut, data[, group[1]], sep="|")
      }
      if(length(facet)>0){
        data$.cut <- paste(data$.cut, data[, facet[1]], sep="|")
      }
      if(length(facet)>1){
        data$.cut <- paste(data$.cut, data[, facet[2]], sep="|")
      }
      data$.cut <- paste(data$.cut, "|", sep="")
    }

    #################
    # could remove some of above with checkTubeData ??
    ##################

    ##################
    #only does method one at a time at moment...
    # maybe warn or stop if multiples...
    ##################
    method <- as.character(method)
    .check <- 1:4 # available methods
    if(!method %in% .check){
      stop("[testTubePrecision]> '", method, "' unknown method",
           "\n\trecommend one of: ", paste (.check, collapse = ", "),
           "\n\t(and maybe check ?testTubePrecision) \n",
           call.=FALSE)
    }

    # calcs
    ##################################
    # (assuming data tagged properly)

    #####################################
    # note (might need watching...)
    # below sort overrides any factors, etc...
    ls <- lapply(sort(unique(data$.cut)), function(z){
      dat.ans <- data[data$.cut==z,]
      ..test. <- c(".start_date",
                ".end_date", ".sample_id", ".cut", group,
                facet)
      #moving to data.table
      #test <- dplyr::group_by(dat.ans, dplyr::pick(test))
      #calling ..test. while using data.table because it
      #seems to be tracking data$test from by...
      #    think need more thinking about
      #    have similar with getTubeX...
      dat.ans <- data.table::as.data.table(dat.ans)
      if(method=="1"){
        ..test. <- dat.ans[, .(.n = length(.tube[is.finite(.tube)]),
                            .mean = mean(.tube, na.rm=TRUE),
                            .low = quantile(.tube, 0.05, na.rm=TRUE),
                            .high = quantile(.tube, 0.95, na.rm=TRUE)
        ), by=..test.]
        test <- as.data.frame(..test.)
      }
      if(method=="2"){
        ..test. <- dat.ans[, .(.n = length(.tube[is.finite(.tube)]),
                            .mean = mean(.tube, na.rm=TRUE),
                            .sd = sd(.tube, na.rm=TRUE)
        ), by=..test.]
        test <- as.data.frame(..test.)
        .ts <- suppressWarnings(qt(0.05/2, df=test$.n-1, lower.tail = FALSE))
        test$.low <- ifelse(test$.n>2, test$.mean - (.ts * (test$.sd/sqrt(test$.n))), NA)
        test$.high <- ifelse(test$.n>2, test$.mean + (.ts * (test$.sd/sqrt(test$.n))), NA)
      }
      if(method=="3"){
        ..test. <- dat.ans[, .(.n = length(.tube[is.finite(.tube)]),
                            .mean = mean(.tube, na.rm=TRUE),
                            .sd = sd(.tube, na.rm=TRUE)
        ), by=..test.]
        test <- as.data.frame(..test.)
        test$.low <- ifelse(test$.n>2, test$.mean - (1.96*test$.sd/sqrt(test$.n)), NA)
        test$.high <- ifelse(test$.n>2, test$.mean + (1.96*test$.sd/sqrt(test$.n)), NA)
      }
      if(method=="4"){
        ..test. <- dat.ans[, .(.n = length(.tube[is.finite(.tube)]),
                            .mean = mean(.tube, na.rm=TRUE),
                            .sd = sd(.tube, na.rm=TRUE)
        ), by=..test.]
        test <- as.data.frame(..test.)
        test$.low <- ifelse(test$.n>2, test$.mean - (qnorm(0.975)*test$.sd), NA)
        test$.high <- ifelse(test$.n>2, test$.mean + (qnorm(0.975)*test$.sd), NA)
      }

      test <- as.data.frame(test)
      dat.ans <- as.data.frame(dat.ans)

      test <- merge(dat.ans, test)    #a join or data.table.merge might be faster ???
      test <- subset(test, .n==n)     #subset for data with replicates
      test <- test[!is.na(test$.tube),]
      if(nrow(test) >= n){
        # if not, not enough data for test
        #models
        mod <- loess(.tube ~ .mean, data=test)
        test$.y <- predict(mod)
        mod.low <- loess(.low ~ .mean, data=test)
        test$.ylow <- predict(mod.low)
        mod.high <- loess(.high ~ .mean, data=test)
        test$.yhigh <- predict(mod.high)
        #lookup
        temp <- (1:100) *10
        temp <- temp[temp >= min(test$.mean) & temp <= max(test$.mean)]
        #testing
        #lookup <- test[1, c(".cut", .xargs$group, .xargs$facet)]
        # lookup <- data.frame(lookup, ans = temp)
        lookup <- data.frame(.cut=test$.cut[1]
                             , ans = temp)
        lookup$low <- predict(mod.low,
                              newdata=data.frame(.mean=lookup$ans))
        lookup$high <- predict(mod.high,
                               newdata=data.frame(.mean=lookup$ans))
        lookup$low.pc <- ((lookup$low-lookup$ans)/lookup$ans)*100
        lookup$high.pc <- ((lookup$high-lookup$ans)/lookup$ans)*100
        #report
        rep <- paste("  ", z, " ",
                     "mean: ", signif(mean(test$.mean, na.rm=TRUE), 4),
                     " (", signif(min(test$.mean, na.rm=TRUE), 4),
                     " to ", signif(max(test$.mean, na.rm=TRUE), 4),
                     ") precision: ",
                     signif(mean((((test$.low - test$.mean)/test$.mean)*100),
                                 na.rm=TRUE), 4), "[%] to ",
                     signif(mean((((test$.high - test$.mean)/test$.mean)*100),
                                 na.rm=TRUE), 4), "[%]",
                     sep="", collapse="")

        ################################
        #this will need to go somewhere
        ################################
        #stop("testTubePrecision: insufficient replicates for test",
        #     call.=FALSE)
      } else {
        lookup <- data.frame(.cut=z, ans = NA, low=NA, high=NA,
                             low.pc=NA, high.pc=NA)
        rep <- paste("  ", z, " Insufficient replicates...",
                     sep="", collapse="")

      }
      list(data=test, lookup=lookup, report=rep)
    })

    #repack lists
    test <- lapply(ls, function(x){
      x$data
    })
    test <- do.call(rbind, test)
    lookup <- lapply(ls, function(x){
      x$lookup
    })
    lookup <- do.call(rbind, lookup)
    rep <- lapply(ls, function(x){
      x$report
    })
    rep <- paste(unlist(rep), collapse="\n")
    rep <- paste("'", tube, "' (rep = ", n, " subset):", "\n",
                 rep, sep="", collapse="")

    #plot
    if(nrow(test)> n){
      ###############################
      # note
      ###############################
      # control for the 5%, 50%, 95% lines
      #   not a right name but line.col works for now
      #       going through tubePlot at moment ... !!
      #       other option pull out line info
      #           pass rest to plot then do the line
      #           stuff in next steps...
      # not documenting until we sort this...
      if(!"line.col" %in% names(.xargs)){
        .xargs$line.col <- "red"
      }
      plt <- do.call(tubePlot,
                     modifyList(.xargs, list(data=test,
                                             x=".mean", y=".tube"))) +
        ggplot2::geom_line(ggplot2::aes(y=.y), col=.xargs$line.col) +
        ggplot2::geom_line(ggplot2::aes(y=.ylow), col=.xargs$line.col,
                           linetype="dashed") +
        ggplot2::geom_line(ggplot2::aes(y=.yhigh), col=.xargs$line.col,
                           linetype="dashed")
    } else {
      plt <- NULL
    }

    #shows
    if("plot" %in% tolower(show)){
      if(!is.null(plt)){
        plot(plt)
      }
    }
    if("summary.report" %in% tolower(show)){
      temp <-  strsplit(rep, "\n")[[1]]
      temp <- temp[!grepl("Insufficient replicates", temp)]
      if(length(temp)>1){
        message(paste(temp, collapse="\n"))
      } else {
        message("not enough replicated data...")
      }
    }
    if("report" %in% tolower(show)){
      message(rep)
    }

    #output
    #######################
    #might not all being staying...
    out <- list(data=test, plot=plt, lookup=lookup, report=rep)
    return(invisible(out))

  }



