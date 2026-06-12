#########################################################
#' @title Miscellaneous Annual Metrics
#########################################################

# this may need rethinking when others start using it...

#' @name misc.tube.annual
#' @aliases misc.tube.annual tubeAnnualCover tubeAnnualTest
#' @description Miscellaneous functions for the calculation of annual
#' diffusion tube metrics. By default, these are setup for use with
#' \code{DTEval}, so expect tagged data (see \code{\link{tagTube}}),
#' and are intended to be applied by year and location to provide
#' information about local data coverage (or capture efficiency)
#' independent of co-locational sampling or site re-naming.

# common annual stats calculators...

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param tube The \code{data} column containing the diffusion tube
#' measurements, by default \code{.value}.
#' @param by The \code{data} column containing the site or local codes to
#' be used when grouping \code{tube} values for annual statistic
#' calculation, by default \code{.location}.
#' @param ... additional arguments, e.g. \code{output} and \code{rename}
#' which allow users to modify function outputs.
#' @param test (\code{tubeAnnualTest} only) A test to be evaluated based on
#' data-series in \code{data}, supplied as a character string.
#' @param years (\code{tubeAnnualTest} only) The years the test should be
#' applied to if not the full range of the data.


#' @details
#' \code{tubeAnnualCover} attempts to calculate the annual data coverage,
#' based of the \code{data} \code{.start_date} and \code{.end_date} tags.
#' By default, it is binned and averaged by \code{.location} to reduce issues
#' associated with replicate sampling and site renaming, and calculates two
#' metrics: \code{n}, the number of days measurements have been
#' reported at that location in the year the sample was collected; and,
#' \code{pc}, the percent of the year that accounts for.
#'
#' \code{tubeAnnualTest} can be used to generate logical conditioning
#' based upon \code{tubeAnnualCover} outputs. It uses a \code{test}
#' argument, supplied as a string, to generate a column of \code{TRUE},
#' \code{FALSE} responses, and can be applied to the full \code{data}
#' set, or limited to specific years using the extra argument
#' \code{years}.
#'
#' All functions assume \code{data} is data \code{DTEval} will recognise
#' as diffusion tube data, i.e., previously tagged (see
#' \code{\link{tagTube}}).


#' @return
#' By default \code{tubeAnnualCover} returns \code{data} with two
#' additional columns, \code{.annual.n} the numbers of days with
#' at least one measurement reported for that location in that year,
#' and \code{.annual.n} the percentage of days with at least one
#' measurement reported for that location in that year.
#'
#' By default \code{tubeAnnualTest} returns \code{data} with one
#' additional column, the logical response to the supplied test, for
#' example a call including the argument \code{test='.annual.n>300'}
#' will, assuming your \code{data} contains a column called
#' \code{.annual.n}, generate a column, \code{TRUE} for all
#' case where all \code{.annual.n} entries for the same year and
#' location were larger than 300 or \code{FALSE} if not.


# examples of decide
# may want something not run to illustrate a lot of this

## #' @examples
## #' dt <- tubeAnnualCover(dt.brd, rename=c("year.days", "percent.year"))
## #' dt <- tubeAnnualTest(dt, test="percent.days>=75", years=2022:2024)




# might be become a tagTubeAnnualCover, etc, and tag and a calc version...

# this is currently hardcoded as by .location;
#      with .annual.n and .annual.pc as outputs
#      do we want rename on this ?

# like a subset or grouping function to do good.2022.2024, etc
#    maybe something like tubeInXYPolygon ???
#    following similar logic it should go in misc.tube.cover

# example code for TRUE/FALSE column from previous work...

# old method for 75pc thresholds
#    this was five sites just not sure why...

## ans <- ans[annual.pc >= 75, ]
## ggplot(ans) + geom_histogram(aes(x=.year, fill=.location), col=NA, stat="count") +
##      theme(legend.position = "none")
## ans <- ans[,.(good.cover.2022.4=all(c("2022", "2023", "2024") %in% .year),
##              good.cover.2022.5=all(c("2022", "2023", "2024", "2025") %in% .year)),
##           by=c(".location")]
## d2 <- merge(d2, as.data.frame(ans))

##  # 75pc thresholds
##ans <- ans[.annual.pc >= 75, ]
## ggplot(ans) + geom_histogram(aes(x=.year, fill=.location), col=NA, stat="count") +
##      theme(legend.position = "none")
##ans <- ans[,.(good.cover.2022.4=all(c("2022", "2023", "2024") %in% .year),
##              good.cover.2022.5=all(c("2022", "2023", "2024", "2025") %in% .year)),
##           by=c(".location")]
#dt <- merge(tagTubeYear(dt), as.data.frame(ans))
##print(names(ans))
##test <- ans[good.cover.2022.5==TRUE,]
##test <- unique(as.data.frame(test)$.location)
##dt <- as.data.frame(dt)
##dt$good.cover.2022.5 <- dt$.location %in% test
##as.data.frame(dt)


#' @rdname misc.tube.annual
#' @export

tubeAnnualCover <- function(data, tube = ".value", by=".location",
                            ...){

  # decide tube and by defaults, handling, etc...
  # might need more args...
  .xargs <- modifyList(list(output = c("n", "pc"),
                            meta = FALSE),
                       list(...))
  if(!all(.xargs$output %in% c("n", "pc"))){
    stop("[tubeAnnualCount] bad output requested...",
         call. = FALSE)
  }
  .xargs$output <- paste(".annual.", .xargs$output, sep="")
  if(!is.null(.xargs$rename) && length(.xargs$rename) != length(.xargs$output)){
    stop("[tubeAnnualCount] output/rename mismatch, lengths differ...",
         call. = FALSE)
  }

  # tag data - testing required tagging only
  d2 <- tagTubeRequired(data, required=c(tube, by), ...)
  d2 <- tagTubeStartEnd(d2)

  # need tube for the .location average
  # MAY also want for if we need to test for NAs once padding is running???

  # convert to by-based table
  #     do we replace .location with by below?
  temp <- calcTubeStat(d2, tube, by=c(".start_date", ".end_date",
                                      ".year", by))
  temp <- data.table::as.data.table(temp)

  #calculate coverage by location
  #     do we replace .location with by below?
  .fun_testCapture <- function(str, stp){
    #.str <- min(str, na.rm=TRUE)
    #.stp <- max(stp, na.rm=TRUE)
    #.seq <- seq(.str, .stp)
    .ref <- lapply(1:length(str), function(x){
      seq(str[x], stp[x], na.rm=TRUE)
    })
    .ref <- sort(unique(do.call(c, .ref)))
    length(.ref)
  }
  ans <- temp[, .(.annual.n=.fun_testCapture(.start_date, .end_date)
  ),
  by=c(".year", by)]

  # minor issue this assumes all years are 365 days
  #    BUT obviously there are leap years...
  ans <- ans[, .annual.pc := .annual.n/365 * 100]

  # drop unwanted outputs
  ####################################
  # this need reworking
  #   currently wont work with to rename
  ###################################
  if(!".annual.n" %in% .xargs$output){
    ans <- ans[, .annual.n := NULL]
  }
  if(!".annual.pc" %in% .xargs$output){
    ans <- ans[, .annual.pc := NULL]
  }
  #rename
  if(!is.null(.xargs$rename)){
    for(i in 1:length(.xargs$rename)){
      if(.xargs$output[i] %in% names(ans)){
        names(ans)[names(ans)==.xargs$output[i]] <- .xargs$rename[i]
      }
    }
  }

  #meta extract shortcut
  if(.xargs$meta){
    return(as.data.frame(ans))
  }

  # rebuild and return
  dt <- merge(tagTubeYear(d2), as.data.frame(ans), by=c(".year", ".location"))
  return(as.data.frame(dt))

}



#' @rdname misc.tube.annual
#' @export

tubeAnnualTest <- function(data, test=NULL, by=".location",
                           years = NULL,
                            ...){

  .xargs <- list(...)

  # tag data - testing required tagging only
  d2 <- checkTubeData(data, test, if.err="stop<<tubeAnnualTest>>test")
  d2 <- tagTubeRequired(d2, required=c(by, ".year"), ...)

  if(is.null(years)){
    years <- unique(tagTubeYear(d2)$.year)
  }
  if(!is.character(years)){
    years <- as.character(years)
  }
  years <- sort(years)
  .start <- years[1]
  .end <- years[length(years)]

  .test <- d2[, test]
  .years <- as.character(years)
  ans <- data.table::as.data.table(d2[.test,])
  ans <- ans[,.(..annual.test..=all(.years %in% .year)),
             by=c(by)]
  if("rename" %in% names(.xargs)) {
    names(ans)[names(ans) %in% "..annual.test.."] <- .xargs$rename[1]
  } else {
    .temp <- paste(test, .start, .end, sep=".")
    names(ans)[names(ans) %in% "..annual.test.."] <- .temp
  }

  ##########################
  # give meta back here
  #     if any reason to give it back???
  ##########################

  ans <- as.data.frame(ans)
  .test <- ans[ans[,ncol(ans)],]

  d2 <- d2[, names(d2) != test]
  d2$..annual.test.. <- 0

  for(i in 1:(ncol(.test)-1)){
    .nm <- names(.test)[i]
    .temp <- unique(.test[,.nm])
    d2$..annual.test.. <- d2$..annual.test.. +
      ifelse(d2[,.nm] %in% .temp, 1, 0)
  }

  d2$..annual.test.. <- d2$..annual.test.. == (ncol(ans)-1)
  if("rename" %in% names(.xargs)) {
    names(d2)[names(d2) %in% "..annual.test.."] <- .xargs$rename[1]
  } else {
    .temp <- paste(test, .start, .end, sep=".")
    names(d2)[names(d2) %in% "..annual.test.."] <- .temp
  }

  return(as.data.frame(d2))
}



