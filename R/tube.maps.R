############################################
#' @title Diffusion Tube maps
############################################

#' @name tube.maps
#' @aliases tube.maps
#' @description \code{DTEval} currently provides static \code{ggplot2} and
#' dynamic (html) \code{leaflet} diffusion tube maps.
#' @param data Data source, typically a data.frame or similar, to be used to
#' build a plot using ggplot2.
#' @param x,y The names of the data-series to plot on the
#'  X and Y axes, respectively.
#' @param polygon (Optional) Polygon of boundary records, e.g.
#' CAZ boundaries.
#' @param plot.type Type of plot generated, current options are 'leaflet'
#' and 'ggplot2'.
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


#' @return \code{tubeMap_leaflet} and \code{tubeMap_leaflet} return
#' \code{leaftlet} and \code{ggplot2} maps, respectively. \code{tubeMap}
#' is wrapper for both.


#############################
# notes
#############################

# removed full imports
## #' @import dplyr, etc
## now specifying directly in code, ggplot2::ggplot, etc

# don't use is.null(.xargs$facet) to test for facet in ...
# because it partial matches to longer facet... names if facet not there!!!

# same for groups, etc...




###########################
# tubeMap
###########################

# draw maps of tubes

#' @rdname tube.maps
#' @export

# draw maps of tubes using leaflet...

# when more stable we can
#      standardise a lot of handling in tubeMap before passing to the
#           leaflet or ggplot2 sub-functions...

# do we need to export the tubeMap sub_functions???

# both plot types
#   allow user control of:
#     lat/long assignment... but maybe did it ???
#     maps source (and token?)
#     point colours (and by group and by facet)
#     groups and facets...
#     polygon colours
#     methods for adding extra layers using conventional ggplot2 or leaflet
#         for example adding aurn data points and labels....

# ggplot2
#   hide tranform message...

# leaflet
#   make static copy ....


tubeMap <-
  function(data, x=NULL, y=NULL, polygon=NULL, plot.type = "", ...){

    d2 <- tagTube(data)
    .check  <- c("leaflet", "ggplot2")
    if(!plot.type[1] %in% .check){
      stop("[tubeMap] plot.type needs to be one of:",
           paste(.check, sep=",", collapse = ","), call. = FALSE)
    }
    if(plot.type=="leaflet"){
      return(tubeMap_leaflet(d2, x , y, polygon,...))
    }
    if(plot.type=="ggplot2"){
      return(tubeMap_ggplot2(d2, x , y, polygon, ...))
    }
    #should never get here
    stop("[tubeMap] not clever...", call. = FALSE)

  }




# currently including a leaflet and ggplot/OpenStreetMap version
# maybe merge as one function with plot.type option?

#' @rdname tube.maps
#' @export

# draw maps of tubes using leaflet...

tubeMap_leaflet <-
  function(data, x=NULL, y=NULL, polygon=NULL, ...){

    #plot only intended for tagged or tag-able data...
    d2 <- tagTube(data)

    # avoiding %>%
    m <- leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.Positron")

    if(!is.null(polygon)){
      m <- leaflet::addPolygons(m, data=polygon)
    }

    m <- leaflet::addCircleMarkers(m, lng=d2$.longitude, lat=d2$.latitude,
                                radius=2)
    #print(m)
    return(m)
  }



#' @rdname tube.maps
#' @export

# draw maps of tubes using ggplot2...

tubeMap_ggplot2 <-
  function(data, x=NULL, y=NULL, polygon=NULL, ...){

    #caz.brd <- as.data.frame(sf::st_coordinates(caz.brd))
    dt <- tagTube(data)
    #on map
    rng.lat <- range(dt$.latitude, na.rm=TRUE)
    rng.lon <- range(dt$.longitude, na.rm=TRUE)

    #adding border
    exp <- function(rng, n=0.1){
      temp <- (rng[2]-rng[1])*n
      c(rng[1]-temp, rng[2]+temp)
    }
    rng.lon <- exp(rng.lon, 0.05)
    rng.lat <- exp(rng.lat, 0.05)

    dc <- OpenStreetMap::openmap(c(rng.lat[2], rng.lon[1]),
                  c(rng.lat[1],rng.lon[2]),
                  zoom = NULL,
                  type =  "esri",
                  mergeTiles = TRUE)

    dc <- OpenStreetMap::openproj(dc, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

    m <- OpenStreetMap::autoplot.OpenStreetMap(dc)

    if(!is.null(polygon)){

      caz.brd <- as.data.frame(sf::st_coordinates(caz.brd))

      m <- m + ggplot2::geom_polygon(ggplot2::aes(X, Y, fill="CAZ"),
                                         col = "blue", fill="blue",
                                         alpha=0.25,
                                         data=caz.brd)
    }

    m <- m + ggplot2::geom_point(data=dt, ggplot2::aes(x=.longitude, y=.latitude))+
      #col=type_laqm)) +
      ggplot2::geom_text(data=data.frame(),
                ggplot2::aes(x=rng.lon[2], y=rng.lat[1]),
                label="Map layer: (c) OpenStreetMap/ESRI contributors    ",
                size=1.8, hjust=1, vjust=-0.5) +
      #scale_fill_manual(name = "",
      #                  values = c("CAZ" = "blue")) +
      ###################
      # should use coord_sf but looks wrong...
      #ggplot2::coord_sf() +
      ggplot2::coord_quickmap() +
      ggplot2::theme_void()

    return(m)
  }


