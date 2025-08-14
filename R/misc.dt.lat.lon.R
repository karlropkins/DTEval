#########################################################
#' @title Miscellaneous Latitude and Longitude Handlers
#########################################################

# this may need rethinking when others start using it...

# also bufferXYPolygon needs writing...
#      probably using sf::st_buffer(polygon, distance)
# see also about spotting suspect lat/lons
#      these about 15 miles and 5 miles as metres
#           tubeMap(dt, polygon= sf::st_buffer(caz.brd, 24500), plot.type="leaflet")
#           tubeMap(dt, polygon= sf::st_buffer(caz.brd, 8200), plot.type="leaflet")
#       if outside suspect ???
# getting ranges data
#       following takes a while and does not quite work....
#            tubeMap_leaflet(dt, polygon= sf::st_buffer(sf::st_as_sf(dt, coords=c(".longitude", ".latitude")), 1))

# note sure we need bufferXYPolygon ???
#      something to make standard data.frames into sp objects might be better, e.g.
#            sf::st_as_sf(dt, coords=c(".longitude", ".latitude"))
#                may then hull or whatever to make into a polygon...


#' @name misc.dt.lat.lon
#' @aliases misc.dt.lat.lon tubeSummaryLatLon tubeInXYPolygon
#' @description Miscellaneous functions for use with Latitude and Longtitude
#' data. By default, these are setup for use with \code{DTEval}, so
#' expects tagged data (see \code{\link{tagTube}}).

# common lat/lon handlers

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param polygon Data source or polygon shape file.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{tubeSummaryLatLon} attempts to calculate the distance between
#' sample locations (unique lat/lon combinations) and the estimated center
#' of sampling, and ranks by distance starting with the most-distance.
#'
#' \code{tubeinXYPolygon} attempts to identify all records in \code{data}
#' that are inside the supplied \code{polygon}. It is a
#' wrapper for \code{splancs} function \code{inout} that uses the latitude
#' and longitude tags of the supplied \code{data}, and the \code{x} and
#' \code{y} columns of the supplied \code{polygon} to test \code{data} rows
#' for inclusion. However, if you want to use different \code{data} or
#' \code{polygon} columns these can be reset using \code{lat} and \code{lon}
#' for the \code{data}-point locations and \code{x} and \code{y} for
#' \code{polygon}.
#'
#' All functions assume \code{data} and \code{polygon} contain point and
#' polygon source information, respectively, that \code{data} is data
#' \code{DTEval} will recognise as diffusion tube data, i.e.,  previously
#' tagged (see \code{\link{tagTube}}), and that these and the x/y values in
#' \code{polygon} are WGS84 format map coordinates.

# not sure above is currently right??

#' @return \code{tubeSummaryLatLon} return a \code{data.frame} of requested
#' \code{data} statistics.
#'
#' By default \code{tubeInXYPolygon} returns \code{data} with an
#' additional column, \code{.in_polygon} that identifies tubes as
#' \code{TRUE} (inside \code{polygon} bounderies) or \code{FALSE}
#' (outside \code{polygon} bounderies).





#############################
# tubeSummaryLatLon
#############################

# standard DT by lat/lon check

# currently does
###############################
#    orders unique non-NA lat/lon combinations by distance from appr. center of data
#        very rough center... (should be better)
#        then uses AQEval findNearLatLon to calculate distance to center
#        then reorder furthermost (first) to nearest (last)

#  thinking about
##############################

#   also other methods
#        1. calculate center better or differently
#              found a couple of methods but not sure they'll make much of a difference at these scales???
#        2. calculate in/out area based on a reference, bbox or circle etc
#              (from misc.dt.lat.lon notes)
#              these about 15 miles and 5 miles as metres
#                   tubeMap(dt, polygon= sf::st_buffer(caz.brd, 24500), plot.type="leaflet")
#                   tubeMap(dt, polygon= sf::st_buffer(caz.brd, 8200), plot.type="leaflet")
#              but what could be used ?? above is 15 and 5 buffers about the caz...
#   playing with as a plot summary
#       ggplot2::ggplot(tubeSummaryLatLon(dt)) + ggplot2::geom_histogram(ggplot2::aes(x=distance.m), bins=220)
#       (bins is nrow for output)

# not sure this is staying


#' @rdname misc.dt.lat.lon
#' @export

tubeSummaryLatLon <- function(data, ...){

  #d2 should have all tags if data is recognisable dt data
  d2 <- tagTube(data)

  # get unique+non-na lat/lon combinations
  d2 <- d2[!duplicated(paste(d2$.latitude, d2$.longitude)), ]
  d2 <- d2[!is.na(d2$.latitude),]
  d2 <- d2[!is.na(d2$.longitude),]

  #estimate lat/lon 'center'
  lat <- median(d2$.latitude, na.rm=TRUE)
  lon <- median(d2$.longitude, na.rm=TRUE)
  # assuming we have valid lat/lon tags...
  out <- AQEval::findNearLatLon(lat, lon, ref = d2, nmax = nrow(d2),
                                rename.ref.lon = ".longitude",
                                rename.ref.lat = ".latitude")
  out <- out[order(out$distance.m, decreasing=TRUE),
             c(".sample_id", ".latitude", ".longitude", "distance.m")]

  return(out)

}






#############################
# tubeInXYPolygon
#############################

# inout wrapper to handle
#   differently name x/y sources in data and polygon
#   data lat/lon NAs

# currently doing
###############################


#  thinking about
##############################
#  handling for different outputs??
#       vector name
#       vector elements/type
#  handling grouping/subsets within polygon
#      for multiple polygon files and for buffering

#' @rdname misc.dt.lat.lon
#' @export

tubeInXYPolygon <- function(data, polygon, ...){

  .xargs <- list(...)

  # do we need/want to force tagging..??
  #    data <- tagTube(data)

  # set up df1 - x/y point source
  df1 <- as.data.frame(data)
  # data lat/lon sources
  lat <- if("lat" %in% names(.xargs)){
    .xargs$lat
  } else {
    ".latitude"
  }
  lon <- if("lon" %in% names(.xargs)){
    .xargs$lon
  } else {
    ".longitude"
  }
  # x and y
  df1 <- checkTubeData(df1, c(lon, lat), if.err="stop<<tubeInXYPolygon>>data lat/lon")
  df1 <- df1[c(lon, lat)]
  names(df1) <- c("x", "y")
  # remove any NAs...
  df1 <- df1[!is.na(df1$x),]
  df1 <- df1[!is.na(df1$y),]
  # only need to do unique lat/lon combinations...
  #     merging them back onto dataset
  df1 <- df1[!duplicated(paste(df1$x, df1$y)),]

  # set up df2 - x/y poly source
  if(class(polygon)[1]=="sf"){
    polygon <- as.data.frame(sf::st_coordinates(polygon))
  }
  # polygon x,y
  x <- if("x" %in% names(.xargs)){
    .xargs$x
  } else {
    "X"
  }
  y <- if("y" %in% names(.xargs)){
    .xargs$y
  } else {
    "Y"
  }
  df2 <- checkTubeData(polygon, c(x,y), if.err="stop<<tubeInXYPolygon>>poly x/y")
  df2 <- df2[c(x,y)]
  names(df2) <- c("x", "y")

  #test
  df1$.in_polygon <- splancs::inout(df1, df2)
  df1 <- df1[c("x", "y", ".in_polygon")]
  names(df1) <- c(lon, lat, ".in_polygon")

  #currently output is hard coded as data + .in_polygon (the T/F/NA column)
  # if we start using merge.data.table for this we need to check dim in versus dim out
  out <- merge(data, df1)

  return(out)

}


