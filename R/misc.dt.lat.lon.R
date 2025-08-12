#########################################################
#' @title Miscellaneous Latitude and Longitude Handlers##
#######################################################

# this needs sorting...

# also bufferPolygon needs writing...

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
#' All functions assumed \code{data} and \code{polygon} contain point and
#' polygon source information, that \code{data} is data \code{DTEval} will
#' recognise as diffusion tube data, so either previously tagged tube data
#' or data that is tag-able using a default call of \code{\link{tagTube}},
#' and that these and the x/y data in \code{polygon} are WGS84 format
#' map coordinates.

#' @return By default \code{tubeInXYPolygon} returns \code{data} with an
#' additional column, \code{.in_polygon} that tube inclusion as \code{TRUE}
#' or \code{FALSE}.



#############################
# tubeInXYPolygon
#############################

# inout wrapper to handle
#   differently labelled x/y sources in data types
#   NAs
#   different output options

# currently doing
###############################
#

#  thinking about
##############################
#

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
  out <- merge(data, df1)

  return(out)

}


