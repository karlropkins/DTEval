############################################
#' @title Diffusion Tube maps
############################################

#' @name tube.maps
#' @aliases tube.maps tubeMap leafletTubeMap
#' @description \code{DTEval} currently provides static \code{ggplot2} and
#' dynamic (html) \code{leaflet} diffusion tube maps.
#' @param data Data source, typically a data.frame or similar, to be used to
#' build a plot using ggplot2.
#' @param x,y The names of the data-series to plot on the
#' X and Y axes, respectively, and assumed to be sampling point longitude and
#' latitude.
#' @param previous (Optional) a previous \code{tubeMap} output to add to,
#' useful if you are plotting lots of different data on the same map and
#' don't want to have to download the map layer every time.
#' @param ... Additional arguments. See Notes

#' @details In addition to \code{data}, the main data source for plots,
#' \code{ggplotTubeShell} handles the following common plot arguments:
#'

# #' * \code{xlab}, \code{ylab} The X and Y axes labels to use if
# #' different from plot defaults.
# #'
# #'  * \code{group} The name of the data-series to use to group
# #'  plotted data with. By default, \code{ggplotTubeShell} assumes
# #'  supplied groups are factors if less than 20 unique cases.
# #'
# #'  * \code{facet} The name of the data-series to use to cut
# #'  the data by when generating multiple plot panels. Also provides
# #'  an addition \code{facet.type} shortcut to different \code{ggplot2}
# #'  facet options: \code{'wrap'} for \code{facet_wrap} (default),
# #'  \code{'grid'} for \code{facet_grid} (or 'grid.row' or grid.col' to
# #'  specific by row-then-column or column-then-row grid handling,
# #'  respectively).

#' @note
#' These functions are in development, so may change.
#'
#' Addition arguments include:
#' * \code{point} to add tube locations as points.
#' * \code{polygon} to add a polygon to the map.
#' * Common plotting arguments, e.g \code{col} for colour, etc. These are
#' typically applied to all plot elements (points, polygons, ect) that
#' can apply them but they can also be specified, e.g. \code{point.col='red'}
#' to just colour points red.


#' @return \code{tubeMap} generates static maps using \code{ggplot2}
#' and \code{OpenStreetMap}. \code{leafletTubeMap} generates interactive
#' (HTML) maps using \code{leaflet}.


#############################
# notes
#############################

# removed full imports
#    now always specifying directly in code, ggplot2::ggplot, etc
#    bit painful but some guessing some loading orders are generating conflicts

# don't use is.null(list(...)$facet), etc to test for facet
#    because it partial matches to longer facet... names if facet not there!!!
# same for groups, etc...



###########################
# tubeMap
###########################

# draw maps of tube locations

# notes
############################

# separated leaflet and ggplot2 versions
#   so this is just ggplot these days...

# current test
#    dd <- tagTube(dont.share::dt.bradford.2); dd <- dd[dd$.longitude<0,]
#    dd$.year <- format(dd$.start_date, "%Y")
#    tubeMap(dd)

#' @rdname tube.maps
#' @export

tubeMap <-
  function(data, x=NULL, y=NULL, ...){

    #setup
    ####################
    #args
    .xargs <- list(...)
    .xargs <- dte_ggshellTidyArgs(.xargs)
    #data
    if("ggplot" %in% class(data)){
      previous <- data
      d2 <- data$data
    } else {
      d2 <- data
      previous <- NULL
    }
    if("newdata" %in% names(.xargs)) {
      d2<-.xargs$newdata
    }

    if(is.null(x)){
      x <- ".longitude"
    }
    if(is.null(y)){
      y <- ".latitude"
    }
    .x <- getTubeX(d2, x, if.err="stop<<tubeMap>>x")
    .y <- getTubeX(d2, y, if.err="stop<<tubeMap>>y")

#############################
# without any this dies if length(names(.xargs)) > 1
#############################
    # currently not facetting tubeMaps
    if(length(names(.xargs))>0 && any(grepl("^facet", names(.xargs)))){
      stop("[tubeMap] Sorry, map facet currently disabled",
           call. = FALSE)
    }
    ########################
    #
    if(!"grid.borders" %in% names(.xargs)){
      .xargs$grid.borders <- 0.05
    }

    ##build map layer
    ###################
    if(is.null(previous)){
      #map ranges
      rng.lat <- range(.y, na.rm=TRUE)
      rng.lon <- range(.x, na.rm=TRUE)
      #adding border
      exp <- function(rng, n=0.1){
        temp <- (rng[2]-rng[1])*n
        c(rng[1]-temp, rng[2]+temp)
      }
      .gb <- .xargs$grid.borders
      rng.lon <- exp(rng.lon, .gb)
      rng.lat <- exp(rng.lat, .gb)
      dc <- OpenStreetMap::openmap(c(rng.lat[2], rng.lon[1]),
                                   c(rng.lat[1],rng.lon[2]),
                                   zoom = NULL,
                                   type =  "esri",
                                   mergeTiles = TRUE)
      dc <- suppressMessages(suppressWarnings(
        OpenStreetMap::openproj(dc,
             projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ))
      map <- OpenStreetMap::autoplot.OpenStreetMap(dc)
      # general map text, formatting and sizing
      suppressMessages(suppressWarnings(
        map <- map + ggplot2::geom_text(data=data.frame(),
                            ggplot2::aes(x=rng.lon[2], y=rng.lat[1]),
                            label="Map layer: (c) OpenStreetMap/ESRI contributors    ",
                            size=1.8, hjust=1, vjust=-0.5) +
                   ggplot2::coord_quickmap() +
                   ggplot2::theme_void()
      ))
      map$data <- d2 # cludge because autoplay not using my data
    } else {
      map <- previous
    }

    if(any(grepl("^polygon", names(.xargs)))){
      ##########################
      #testing this tidy
      #    Holoding code because it is very sensitive to ordering
      #.xargs2 <- .xargs[grepl("^polygon[.]", names(.xargs))]
      #names(.xargs2) <- gsub("polygon[.]", "", names(.xargs2))
      #names(.xargs2)[names(.xargs2) %in% c("col", "color")] <- "colour"
      #.xargs2 <- .xargs2[!duplicated(names(.xargs2), fromLast=TRUE)]
      #.xargs2 <- modifyList(.xargs, .xargs2)
      .xargs2 <- dte_ggshellTidyArgs(.xargs, "polygon")
      if(.xargs2$..test=="OK"){
        .xargs2 <- modifyList(list(x="X", y="Y"), .xargs2)
        ##########################
        # to think about...
        #    this currently needs polygon to be a sf polygon...
        #        BUT polygon could be a different object type ...
        #        OR polygon could be true... then you would use the data as polygon source
        #            can't colour a polygon by palette at moment
        .xargs2$polygon <- as.data.frame(sf::st_coordinates(.xargs2$polygon))
        drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPolygon)]
        map <- dte_ggshellAddGeom(.xargs2, .xargs2$polygon, map,
                                  ggplot2::geom_polygon,
                                  defaults = list(na.rm=TRUE, colour="blue",
                                                  fill="blue", alpha=0.25),
                                  drops = drops)
        }
    }

    # add surface
    if(any(grepl("^surface", names(.xargs)))){
      .xargs2 <- dte_ggshellTidyArgs(.xargs, "surface")
      .xargs2.test <- dte_ggshellTestArgs(.xargs2, d2)
      .test <- names(.xargs2.test[.xargs2.test=="data"])
      .test <- .test[.test %in% c("fill", "z", "colour", "contour")]
      if(length(.test)>0){
        .fit.args <- modifyList(list(data=d2, tube=.xargs2[[.test[1]]],
                                     inputs=c(x,y), by=c(.xargs$group, .xargs$group),
                                      simplify=TRUE, newdata="input.ranges"),
                                .xargs2[names(.xargs2) %in% c("too.far",
                                                              "grid.resolution",
                                                              "grid.borders")])
        .d2 <- do.call(fitTubeModel, .fit.args)
        .xargs2[[.test[1]]] <- paste(.xargs2[[.test[1]]], ".pred", sep="")
        if(.xargs2$..test=="OK"){
          .xargs2 <- modifyList(list(x=x, y=y), .xargs2)
          ##########################
          # if fill in there
          if("fill" %in% names(.xargs2)){
            .xargs2$fill <- .xargs2[[.test[1]]]
            drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomTile)]
            drops <- c(drops, "col", "color", "colour")
            map <- dte_ggshellAddGeom(.xargs2, .d2, map,
                                      ggplot2::geom_tile,
                                      defaults = list(na.rm=TRUE, alpha=0.5),
                                      drops = drops)
          }
          if("contour" %in% names(.xargs2)){
            .xargs2$z <- .xargs2[[.test[1]]]
            drops <-  names(.xargs2)[!names(.xargs2) %in% c(dte_GeomArgs(ggplot2::GeomContour),
                                                            "z")]
            drops <- c(drops, "fill")
            map <- dte_ggshellAddGeom(.xargs2, .d2, map,
                                      ggplot2::geom_contour,
                                      defaults = list(na.rm=TRUE,
                                                      colour="white"),
                                      drops = drops)
            map <- dte_ggshellAddGeom(.xargs2, .d2, map,
                                      metR::geom_text_contour,
                                      defaults = list(na.rm=TRUE,
                                                      colour="white"),
                                      drops = drops)
          }

        }

      }

    }

    #add point(s)
    if(any(grepl("^point", names(.xargs)))){
      ##########################
      # testing this tidy
      .xargs2 <- dte_ggshellTidyArgs(.xargs, "point")
      if(.xargs2$..test=="OK"){
        .xargs2 <- modifyList(list(x=x, y=y), .xargs2)
        ##########################
        drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPoint)]
        map <- dte_ggshellAddGeom(.xargs2, d2, map,
                                  ggplot2::geom_point,
                                  defaults = list(na.rm=TRUE),
                                  drops = drops)
      }
    }

    ##################
    # to do
    #################
    # point mean, count, etc...
    # add faceting
    # add grouping ???
    # add surface layer
    # add/tidy structure - palette, legend, ect...

    #
    return(map)
  }







##############################
# leafletTubeMap
##############################

# draw maps of tubes using leaflet...

# separated leaftlet and ggplot2 versions of tubeMap 2025-09-18
#

# notes
##########################

# both plot types
#   allow user control of:
#     lat/long assignment... but maybe did it in previous version ???
#     maps source (and token?)
#     point colours (and by group and by facet)
#     groups and facets...
#     polygon colours
#     methods for adding extra layers using conventional ggplot2 or leaflet
#         for example adding aurn data points and labels....

# jobs
#############################

# leaflet
#   make static copy ....

# current test
#    dd <- tagTube(dont.share::dt.bradford.2); dd <- dd[dd$.longitude<0,]
#    leafletTubeMap(dd, point=T, polygon=dont.share::caz.bradford)

# jobs
#    standardize args and options so it is more like tubeMap
#    add surface fit



#' @rdname tube.maps
#' @export

# draw maps of tubes using leaflet...

leafletTubeMap <-
  function(data, x=NULL, y=NULL, previous=NULL, ...){

    #plot only intended for tagged or tag-able data...
    #do we need/want tagging ?
    d2 <- tagTube(data)
    .xargs <- list(...)

    ############################
    # x and y handling to enable
    ############################

    # avoiding %>%
    m <- if(is.null(previous)){
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron")
    } else {
      previous
    }

    if("polygon" %in% names(.xargs)){
      m <- leaflet::addPolygons(m, data=.xargs$polygon)
    }

    if("point" %in% names(.xargs)){
      m <- leaflet::addCircleMarkers(m, lng=d2$.longitude, lat=d2$.latitude,
                                     radius=2)
    }

    #print(m)
    return(m)
  }





