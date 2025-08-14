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

#' @name misc.dt.lat.lon
#' @aliases misc.dt.lat.lon tubeInXYPolygon
#' @description Miscellaneous functions for use with Latitude and Longtitude
#' data. By default, these are setup for use with \code{DTEval}, so
#' expect tagged data (see \code{\link{tagTube}}).

# common lat/lon handlers

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param polygon Data source or polygon shape file.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{TubeinXYPolygon} attempts to identify all records in \code{data},
#' that are within the boundaries of the supplied \code{polygon}. It is a
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

# not sure above is currently right?? not tag-able
#     by default it assumed

#' @return By default \code{tubeInXYPolygon} returns \code{data} with an
#' additional column, \code{.in_polygon} that tube inclusion as \code{TRUE}
#' or \code{FALSE}.



#############################
# tubeInXYPolygon
#############################

# inout wrapper to handle
#   differently name x/y sources in data and polygon
#   data lat/lon NAs

# currently doing
###############################
#  handling for different outputs??
#       vector name
#       vector elements/type


#  thinking about
##############################
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


