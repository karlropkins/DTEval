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
#' @param by The name(s) of hierarchical grouping terms. These are used to
#' sub-sample the data before clustering data.
#' @param clusters The number of clusters to extract, default 2.
#' @param method The clustering method to apply: 1 (standard cluster);
#' 2 (clustering of correlations); or 3 (clustering on normalised profiles)
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{clusterTubeData} attempts to cluster data in the supplied time-series.
#'
#' It assigns data to the requested number of clusters.
#'
#' @note \code{clusterTubeData} and related functions assume that \code{data}
#' is a data set \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return the supplied \code{data.frame}
#' with attached predicted time-series components: \code{..season},
#' \code{..deseason}, etc.

# need to check that data is not a simiplifed version of the supplied data...

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

# example????
##############################


#' @rdname cluster.tube
#' @export

# this uses cluster package...

clusterTubeData <- function(data, tube=".value", by="site",
                            clusters=2, method=2, ...){

  #setup
  d2 <- tagTube(data)
  d2 <- tagTubeDate(d2)
  d2 <- checkTubeData(d2, tube, if.err="stop<<clusterTubeData>>tube")
  d2 <- checkTubeData(d2, by, if.err="stop<<clusterTubeData>>by")

  #reshape
  .temp <- ".date"
  if(length(by)>1){
    .temp <- paste(c(.temp, by[2:length(by)]), collapse ="+")
  }
  .temp <- as.formula(paste(.temp, "~", by[1], sep=""))
  d2 <- data.table::as.data.table(d2)
  d2 <- data.table::dcast.data.table(d2, .temp,
                                     fun.aggregate = function(x){mean(x, na.rm=TRUE)},
                                     value.var=tube)
  .temp <- ".date"
  if(length(.temp)>1){
    .temp <- c(.temp, by[2:length(by)])
  }

  if(method==1){
    d2 <- d2[, c(.temp) := NULL]
    d2 <- as.data.frame(d2)
    #return(d2)
    clst <- cluster::clara(t(d2), clusters, correct.d=TRUE)
    .temp <- data.frame(x=names(clst$clustering),
                        .cluster=factor(clst$clustering))
    names(.temp)[1] <- by[1]
    out <- data.table::merge.data.table(data, .temp, by=by[1])
  }
  if(method==2){
    d2 <- cor(d2[, c(.temp) := NULL],
              use="pairwise.complete.obs")
    d2 <- 1- d2 # in method but not sure what it does when clustering ??
    clst <- cluster::clara(d2, clusters, correct.d=TRUE)
    .temp <- data.frame(x=names(clst$clustering), .cluster=factor(clst$clustering))
    names(.temp)[1] <- by[1]
    out <- data.table::merge.data.table(data, .temp, by=by[1])
  }
  if(method==3){
    d2 <- d2[, c(.temp) := NULL]
    d2 <- as.data.frame(d2)
    .temp <- names(d2)
    d2 <- as.data.frame(apply(d2,2,scale))
    d2 <- as.data.frame(apply(d2,2,diff))
    names(d2) <- .temp
    clst <- cluster::clara(t(d2), clusters, correct.d=TRUE)
    .temp <- data.frame(x=names(clst$clustering),
                        .cluster=factor(clst$clustering))
    names(.temp)[1] <- by[1]
    out <- data.table::merge.data.table(data, .temp, by=by[1])

  }

  # output
  #  like option to export clst
  out
}
