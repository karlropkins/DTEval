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


#' @name misc.tube.lat.lon
#' @aliases misc.tube.lat.lon tubeInXYPolygon
#' @description Miscellaneous functions for use with Latitude and Longtitude
#' data. By default, these are setup for use with \code{DTEval}, so
#' expects tagged data (see \code{\link{tagTube}}).

# common lat/lon handlers

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param polygon Data source or polygon shape file.
#' @param ... additional arguments, currently ignored.

#' @details
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

#' @return
#' By default \code{tubeInXYPolygon} returns \code{data} with an
#' additional column, \code{.in_polygon} that identifies tubes as
#' \code{TRUE} (inside \code{polygon} bounderies) or \code{FALSE}
#' (outside \code{polygon} bounderies).



#############################
# tubeInXYPolygon
#############################

# inout wrapper to handle
#   differently name x/y sources in data and polygon
#   data lat/lon NAs

# currently doing
###############################

# sorting rename/output combination calls...

#  thinking about
##############################

# check this out when time...
# https://stackoverflow.com/questions/59144491/r-unusual-error-plotting-multipolygons-with-ggplot-geom-sf-and-openstreetmap

# muiltpolygon handling

##library(sf)
##myshape <- st_read(system.file("shape/nc.shp", package = "sf"))
##bbox_list <- lapply(st_geometry(myshape), st_bbox)
##polys_list <- lapply(bbox_list, st_as_sfc)
##endpol = polys_list[[1]]
##for (i in 2:length(polys_list)) {
##  endpol <- c(endpol, polys_list[[i]])
##}
##st_crs(endpol) <- st_crs(myshape)
##endpol
## #Plots
##plot(st_geometry(myshape))
##plot(endpol, add=TRUE, border="red")
## #or..
## ggplot2::ggplot(sf::st_buffer(dont.share::caz.bradford, 200)) + ggplot2::geom_sf(, fill=c("red"))

## tubeInXYPolygon does not do holes, etc...

#  coordinate handling
#     added st_transform to polygon handling because some of supplied
#         polygons are not lat/lon (like-wot-was-requested)
#     MIGHT have to do similar elsewhere...

#  handling for different inputs ??
#     currently allows you to rename
#         data latitude and longitude names using lat and lon
#         polygon x and y names using x and y

#  handling for different outputs??
#       time-series/column name added to return data.frame
#           currently default .in_polygon; or rename to reset
#       vector elements/type
#            currently returning TRUE/FALSE/NA as .in_polygon(/rename)
#            currently returning polygon.index/NA as .in_polygon(/rename).id
#            could extended this to content of matched polygon ???

#  handling grouping/subsets within polygon
#      for multiple polygon files and for buffering


## dd <- tagTube(dont.share::dt.bradford.2); dd <- dd[dd$longitude<0,]
## dd <- tubeInXYPolygon(dd, dont.share::caz.bradford)


#' @rdname misc.tube.lat.lon
#' @export

tubeInXYPolygon <- function(data, polygon, ...){

  # set up
  ##########################
  # should we move lat,lon,x,y,crs defaults into next line ???
  # BUT if doing, we need to make sure all are got from .xargs...
  #     either upfront or throughout function...
  .xargs <- modifyList(list(output="ans", rename=".in_polygon"),
                       list(...))
  if(any(!.xargs$output %in% c("ans", "id"))){
    #asking for other outputs
    if(length(.xargs$output)!=length(.xargs$rename)){
      .xargs$rename <- c(.xargs$rename,
                         .xargs$output[!.xargs$output %in% c("ans", "id")])
    }
  }
  if(length(.xargs$output)!=length(.xargs$rename)){
    stop("[tubeInXYPolygon] output/rename mismatch, lengths differ...",
         call. = FALSE)
  }
  # could also add lat, lon, x, y, crs defaults here...

  # do we need/want to force tagging..??
  #    data <- tagTube(data)

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
  data <- tagTubeRequired(data, required=c(lat, lon), ...)
  ##########################
  # I think I would rather everyone tagged
  #   this feels like it might come back to bit use if we get
  #      lots non-data.frames used as data ...
  ###########################
  # set up df1 - x/y point source
  df1 <- as.data.frame(data)
  # x and y
  df1 <- checkTubeData(df1, c(lon, lat), if.err="stop<<tubeInXYPolygon>>data lat/lon")
  df1 <- df1[c(lon, lat)]
  # remove any NAs...
  ###########################
  # in the DT source files
  #     but sf doesn't like them
  #     ALSO guessing stupid lat/longs will kill this...
  #        but hopefully we'll catch them before here...
  ###########################
  df1 <- df1[!is.na(df1[[lon]]),]
  df1 <- df1[!is.na(df1[[lat]]),]
  # only need to do unique lat/lon combinations...
  #     merging them back onto dataset
  df1 <- df1[!duplicated(paste(df1[[lon]], df1[[lat]])),]
  #########################################
  # flipped previous code
  #   now df1 and polygon sf
  #   so I can force everything to latlong
  #   also swithing splanc::inout to sf::st_within
  #   then switching back to data.frame
  #       :(
  #########################################
  df1 <- sf::st_as_sf(df1, coords=c(lon, lat), crs="WGS84")

  # set up df2 - x/y poly source
  if(class(polygon)[1]!="sf"){
    ##############################
    # not tested since shift to sf
    #    ....
    ##############################
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
    crs <- if("crs" %in% names(.xargs)){
      .xargs$crs
    } else {
      "WGS84"
    }
    df2 <- checkTubeData(polygon, c(x,y),
                         if.err="stop<<tubeInXYPolygon>>poly x/y")
    df2 <- sf::st_cast(
      sf::st_transform(
        sf::st_as_sf(df2, coords=c(x, y), crs=crs),
        crs="WGS84"),
      "MULTIPOLYGON")
  } else {
    df2 <- sf::st_cast(sf::st_transform(polygon, crs="WGS84"), "MULTIPOLYGON")
  }

  #output common name
  #just rename now but can be different lengths!!
  #.name <- if("rename" %in% names(.xargs)){
  #  .xargs$rename
  #} else {
  #  ".in_polygon"
  #}
  .tt <- sf::st_within(df1, df2)
  df1 <- as.data.frame(sf::st_coordinates(sf::st_transform(df1, "WGS84")))
  names(df1)[1:2] <- c(lon, lat)
  #################################
  # the next bit still needs work
  #################################
  .tt <- do.call(c, lapply(.tt, function(x){ifelse(any(x>0), x, NA)}))
  if("ans" %in% .xargs$output){
    .name <- .xargs$rename[.xargs$output=="ans"]
    df1[[.name]] <- !is.na(.tt)
  }
  if("id" %in% .xargs$output){
    .name <- .xargs$rename[.xargs$output=="id"]
    df1[[paste(.name, ".id", sep="")]] <- .tt
  }
  ################################
  # other outputs
  ################################
  # adds these if they can be found in df2
  # NOTE: not fully tested with longer
  #       output and/or rename options....
  .ref <- names(df2)
  .ref <- .ref[!.ref %in% c("name","geom")]
  for(i in .ref){
    if(i %in% .xargs$output){
      .name <- .xargs$rename[.xargs$output==i]
      df1[[.name]] <- df2[[i]][.tt]
    }
  }


#  df1[[paste(.name, ".id", sep="")]] <- do.call(c, lapply(.tt, function(x){ifelse(any(x>0), x, NA)}))
#  names(df1) <- c(lon, lat, .name, paste(.name, ".id", sep=""))
  ##############################
  # could track arguments from df2 using something like...
  #   df1$$RUC21NM_adj <- df2$RUC21NM_adj[df1$.in_polygon.id]
  #   maybe do as output = c("ans", "id", "all") or names ....
  #        just not sure how wise copying all would be ???
  #             e.g. LAT and LONG from the polygon could confuse things...
  #                  also increasing the chance of another copy name...
  #        all would be names(df2)[!names(df2) %in% c("name" "geom")]
  #        then for each of those the track argument

  # currently default output is data + .in_polygon (or rename) (T/F/NA column)
  #   output arg to modify this
  #      default output = "ans"
  #      output = "id" .in_polygon.id as NA or polygon index in df2/polygon

  # not done all...
  # if we start using merge.data.table for this we need to check dim in versus dim out

  #only merge if something to merge....
  if(ncol(df1)>2){
    #remove any earlier version of data$[name/rename]
    data <- data[names(data)[!names(data) %in% .xargs$rename]]
    out <- merge(data, df1, by=c(lon, lat))
  } else {
    warning("[tubeInXYPolygon] nothing added/updated... maybe check inputs?",
            call. = FALSE)
    out <- data
  }

  #out
  return(out)

}


