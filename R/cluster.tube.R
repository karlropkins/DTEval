#######################################################
#' @title Cluster Diffusion Tube Data
#######################################################

#' @name cluster.tube
#' @aliases cluster.tube clusterTubeData
#' @description Functions for clustering multiple site
#' diffusion tube (DT).

#  see notes on value of clustering

# cluster analsis
# need to acknowldge Matt
# also on NA handling
#  https://stackoverflow.com/questions/10721299/clustering-with-na-values-in-r
# also on trends
#  https://stackoverflow.com/questions/10555800/how-to-cluster-by-trend-instead-of-by-distance-in-r

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param tube The name of the data-series (in \code{data}) to fit a model
#' to (see \code{model}), typically the DT NO2 concentrations (in ug/m3).
#' @param by The name of a hierarchical grouping term. This is used to
#' group the data before clustering. (see Note.)
#' @param clusters The number of clusters to extract, default 2.
#' @param method The clustering method to apply: 1 (standard cluster);
#' 2 (clustering of correlations); or 3 (clustering on normalised profiles)
#' @param ... additional arguments, currently ignored.

#

#' @details
#' \code{clusterTubeData} attempts to cluster data in the supplied time-series.
#'
#' It assigns data to the requested number of clusters.
#'
#' @note \code{clusterTubeData} is currently only fully tested for use with
#' single \code{by} terms. It will accept multiple values, but at this stage
#' outputs for multiple inputs should not be
#'
#' \code{clusterTubeData} and related functions assume that \code{data}
#' is a data set \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return the supplied \code{data.frame}
#' with attached cluster assignment: \code{.cluster}.

# need to check that data is not a simplified version of the supplied data...

# see notes above on clustering

#############################
# clusterTubedata
#############################

# in-development

# need to document

# current test
#    dd <- tagTube(dont.share::dt.bradford.2); dd <- dd[dd$.longitude<0,]
#    dd <- tubeInXYPolygon(dd, dont.share::caz.bradford)
#    ############################
#    # to finish see bradford analysis

# TO DECIDE

# currently does
###############################
#

#  thinking about
##############################

# cluster analysis for profile correlation to write up
#     see also fig 4.21 code 108 eco eval text (by conc and location)
#        also check research emails - there should be a couple refs there....

# might want to allow user to add an argument to set .date e.g.
#     so they can use an equivalent...

# look at why this dies
# dd <- dont.share::dt.bradford.2; dd <- dd[dd$longitude<0,]
# aa <- deseasonTubeData(dd, by=c(".latitude", ".longitude")); names(aa)
# bb <- clusterTubeData(aa, tube=".value.mean", by=c(".longitude", ".latitude"), cluster=2, method=2);names(bb)
# dies with In cor(d2[, `:=`(c(.temp), NULL)], use = "pairwise.complete.obs") : the standard deviation is zero

# look at why this dies
# dd <- dont.share::dt.bradford.2; dd <- dd[dd$longitude<0,]
# aa <- deseasonTubeData(dd, by=c(".location")); names(aa)
# bb <- clusterTubeData(aa, tube="..deseason", by=c(".location"), cluster=2, method=2);names(bb)
# dies with Error in cluster::clara(d2, clusters, correct.d = TRUE) : Observations 212,213 have *only* NAs --> omit them for clustering!


# example????
##############################


#' @rdname cluster.tube
#' @export

# this uses cluster package...

clusterTubeData <- function(data, tube=".value", by="site",
                            clusters=2, method=2, ...){

  #setup
  ##################
  # rename
  .xargs <- modifyList(list(rename=".cluster"), list(...))
  # data tagging
  data <- tagTubeRequired(data, required=c(tube, by, ".date"), ...)
  d2 <- checkTubeData(data, tube, if.err="stop<<clusterTubeData>>tube")
  d2 <- checkTubeData(d2, by, if.err="stop<<clusterTubeData>>by")

  #reshape data
  .temp <- ".date"
  ###############################
  # TO THINK ABOUT/DO
  # Think I need to flip this to use a dummy variable in data for by
  # issue is if by is multiple terms it can build sensibly
  #    BUT it can't always unpack ...
  #       So, maybe add by ..dummy do analysis, merge with data and then remove ..dummy
  #       BUT maybe still won't work ???
  ########################################
  if(length(by)>1){
    .temp <- paste(c(.temp, by[2:length(by)]), collapse ="+")
  }
  .temp <- as.formula(paste(.temp, "~", by[1], sep=""))
  d2 <- data.table::as.data.table(d2)
  d2 <- data.table::dcast.data.table(d2, .temp,
                                     fun.aggregate = function(x){mean(x, na.rm=TRUE)},
                                     value.var=tube)
  if("output" %in% names(.xargs)){
    # common.data might be right name
    # also might not be right place to return data
    #   also added  data below
    #     - the 'what-we-cluster-test-using-requested-method data...
    #     - need to be done once-per-cluster-method...
    if(.xargs$output == "common.data"){
      return(as.data.frame(d2))
    }
  }

  .temp <- ".date"
  if(length(.temp)>1){
    .temp <- c(.temp, by[2:length(by)])
  }

  check <- 1:6
  if(!method %in% check){
    stop("[clusterTubeData] Unknown method, maybe try one of: ",
         paste(check, collapse=","),
         call.=FALSE)
  }
  if(method==1){
    #amounts
    d2 <- d2[, c(.temp) := NULL]
    d2 <- as.data.frame(d2)
    # return d2
    if("output" %in% names(.xargs)){
      if(.xargs$output == "data"){
        return(as.data.frame(t(d2)))
      }
    }
    clst <- cluster::clara(t(d2), clusters, correct.d=TRUE)
    .temp <- data.frame(.x=names(clst$clustering),
                        .cluster=factor(clst$clustering))
  }
  if(method==2){
    #correlation
    d2 <- cor(d2[, c(.temp) := NULL],
              use="pairwise.complete.obs")
    d2 <- 1- d2 # convert to distance ??
    #################
    # NA handling
    d2[is.na(d2)] <- 1
    d2 <- d2[, apply(as.data.frame(d2),1, function(x){!all(x==1)})]
    #################
    # return d2
    if("output" %in% names(.xargs)){
      if(.xargs$output == "data"){
        return(as.data.frame(d2))
      }
    }
    clst <- cluster::clara(d2, clusters, correct.d=TRUE)
    .temp <- data.frame(x=names(clst$clustering), .cluster=factor(clst$clustering))
  }
  if(method==3){
    # testing
    d2 <- d2[, c(.temp) := NULL]
    d2 <- as.data.frame(d2)
    .temp <- names(d2)
    #####################################
    # think about base r versus data.atble
    # apply(df, 2, function(x) {(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))})
    # setDT(df)[ , lapply(.SD, function(x) (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))]

    # scale ???
    d2 <- as.data.frame(apply(d2, 2, function(x) {(x - min(x, na.rm = T)) /
                                        (max(x, na.rm = T) - min(x, na.rm = T))}))
    names(d2) <- .temp
    # does this need/want NA handling like method 2 ??
    # but that might kill it ???
    if("output" %in% names(.xargs)){
      if(.xargs$output == "data"){
        return(as.data.frame(d2))
      }
    }
    clst <- cluster::clara(t(d2), clusters, correct.d=TRUE)
    .temp <- data.frame(x=names(clst$clustering),
                        .cluster=factor(clst$clustering))
  }
  if (method == 4) {
    d2 <- d2[, `:=`(c(.temp), NULL)]
    d2 <- as.data.frame(d2)
    .temp <- names(d2)
    d2 <- as.data.frame(apply(d2, 2, function(x) {x - mean(x, na.rm=TRUE)}))
    d2 <- as.data.frame(apply(d2, 2, function(x) {x / sd(x, na.rm=TRUE)}))
    names(d2) <- .temp
    if("output" %in% names(.xargs)){
      if(.xargs$output == "data"){
        return(as.data.frame(d2))
      }
    }
    clst <- cluster::clara(t(d2), clusters, correct.d = TRUE)
    .temp <- data.frame(x = names(clst$clustering),
                        .cluster = factor(clst$clustering))
  }
  if (method == 5) {
    d2 <- d2[, `:=`(c(.temp), NULL)]
    d2 <- as.data.frame(d2)
    .temp <- names(d2)
    d2 <- as.data.frame(apply(d2, 2, function(x) {x/mean(x, na.rm=TRUE)}))
    names(d2) <- .temp
    if("output" %in% names(.xargs)){
      if(.xargs$output == "data"){
        return(as.data.frame(d2))
      }
    }
    clst <- cluster::clara(t(d2), clusters, correct.d = TRUE)
    .temp <- data.frame(x = names(clst$clustering),
                        .cluster = factor(clst$clustering))
  }
  if (method == 6) {
    .dd <- as.numeric(d2$.date)
    d2 <- d2[, `:=`(c(.temp), NULL)]
    d2 <- as.data.frame(d2)
    .temp <- names(d2)
    d2 <- t(as.data.frame(apply(d2, 2, function(x) {cor(x, .dd, use="pairwise.complete.obs")})))
    d2 <- 1-d2
    names(d2) <- .temp
    if("output" %in% names(.xargs)){
      if(.xargs$output == "data"){
        return(as.data.frame(d2))
      }
    }
    clst <- cluster::clara(t(d2), clusters, correct.d = TRUE)
    .temp <- data.frame(x = names(clst$clustering),
                        .cluster = factor(clst$clustering))
  }

  # this is currently common but might not last / work...
  ##return(.temp)
  names(.temp)[1] <- by[1]
  #rename
  names(.temp)[2] <- .xargs$rename[1]
  data <- data[names(data)[!names(data) %in% .xargs$rename[1]]]
  .temp[,by[1]] <- as(.temp[,by[1]], class(data[,by[1]]))
  out <- data.table::merge.data.table(data, .temp, by=by[1])

  # output
  #  like option to export clst
  out
}
