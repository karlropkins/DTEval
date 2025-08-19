############################################
#' @title Diffusion Tube Data Plots
############################################

#' @name tube.plots
#' @aliases tube.plots tubePlot tubeTimePlot ggplotTubeShell
#' @description \code{DTEval} uses \code{ggplot2} to generate most plots.
#' \code{ggplotTubeShell} builds common plot shells for many of these, and
#' handles some of of the generic plot control. \code{tubePlot} is a
#' wrapper for several commonly used plots. Functions
#' like \code{\link{testTubePrecision}} and \code{\link{testTubePrecision}}
#' also use it to generate  associated plot outputs.
#' @param data Data source, typically a data.frame or similar, to be used to
#' build a plot using ggplot2.
#' @param x,y The names of the data-series to plot on the
#'  X and Y axes, respectively.
#' @param plot.type The type of plot required.
#' @param avg Optional data averaging.
#' @param ... Additional arguments. See details below

#' @details In addition to \code{data}, the main data source for plots,
#' \code{ggplotTubeShell} handles the following common plot arguments:
#'
#' * \code{xlab}, \code{ylab} The X and Y axes labels to use if
#' different from plot defaults.
#'
#'  * \code{group} The name of the data-series to use to group
#'  plotted data with. By default, \code{ggplotTubeShell} assumes
#'  supplied groups are factors if less than 20 unique cases.
#'
#'  * \code{facet} The name of the data-series to use to cut
#'  the data by when generating multiple plot panels. Also provides
#'  an addition \code{facet.type} shortcut to different \code{ggplot2}
#'  facet options: \code{'wrap'} for \code{facet_wrap} (default),
#'  \code{'grid'} for \code{facet_grid} (or 'grid.row' or grid.col' to
#'  specific by row-then-column or column-then-row grid handling,
#'  respectively).



#' @return \code{ggplotTubeShell} returns a ggplot shell the user then
#' needs to add \code{geom}s to, to build a standard plot.
#'
#' @note Maybe I'm already missing \code{qplot}...



# need to
#########################

# reference and link to ggplot2...


#############################
# notes
#############################

# removed full imports
## #' @import dplyr, etc
## now specifying directly in code, ggplot2::ggplot, etc


####################################
# plotTube
####################################

# common plots wrapper

########################
# second draft ???
########################

# could make tubePlot the standard
# then tubeTimePlot a variation that by default sets x  to data
# also link to tubeMap for tubeSitePlot is tubeMap without the map...

#####################
# first draft
#####################

# maybe use ggplotTubeShell for this???

# options for boxplot, point, smooth ect geom type


# playing with

# tubePlot(dont.share::dt.bradford.2, plot.type="time", group="site")
# tubePlot(dont.share::dt.bradford.2, plot.type="time", group="site") + ggplot2::theme(legend.position="none")
# tubePlot(dont.share::dt.bradford.2, plot.type="time", group="site") + ggplot2::aes(col=NULL)

# gave warning for NAs..
# Warning message:
#   Removed 3 rows containing missing values or values outside the scale range (`geom_point()`).

# nice visualizations....
#    https://www.nature.com/articles/s41467-018-03297-7

# see this about irregular data
#    https://stackoverflow.com/questions/37529116/how-to-plot-a-heat-map-with-irregular-data-in-ordinates-in-ggplot

# also
#    tubePlot(dont.share::dt.bradford.2, plot.type="site", palette=c("white", "green")) + ggplot2::geom_density_2d_filled(breaks=10^(0:10)) +ggplot2::xlim(-2.5,-1) + ggplot2::ylim(53.7, 54)

#

# looking at
# tubePlot(dt.york, x=".start_date", y="measurement", plot.type="statribbon", palette="white")
# not tracking groups ...
# tubePlot(dt.york, x=".start_date", y="measurement", plot.type="statribbon", group="month", col="month")
# this sort of works...
# tubePlot(dt.york, x=".start_date", y="measurement", plot.type="statribbon", group="month", col="month")


#' @rdname tube.plots
#' @export

tubePlot <-
  function(data, x=NULL, y=NULL, z=NULL, plot.type="point",
           avg = NULL,
           ...){

    # setup
    .xargs <- list(...)
    if(!is.null(z)){
      data$.value <- getTubeX(data, z, if.err="return.null")
    }
    if(is.null(data$.value)){
      data$.value <- NA
    }
    if(is.null(x) & is.null(y)){
      stop("[tubePlot]> need at least one of 'x' and 'y'...",
           call.=FALSE)
    }
    if(length(plot.type)==1){
      plot.type <- unlist(strsplit(plot.type, ","))
    }
    plot.type <- tolower(gsub(" ", "", plot.type))

    if(!is.null(avg)){
      data <- suppressWarnings(calcTubeStat(data, ".value", by =c(x, y,
                                          .xargs$facet,
                                          .xargs$group)))
      names(data) <- gsub("[.]mean", "", names(data))
    }

    #plot
    out <- ggplotTubeShell(data, x, y, ...)
    #loop through plot options
    test <-c()
    for(i in plot.type){
      temp <- FALSE
      if(tolower(i)=="point") {
        out <- out + ggplot2::geom_point(na.rm=TRUE)
        temp <- TRUE
      }
      if(tolower(i)=="density") {
        out <- out + suppressWarnings(ggplot2::geom_density_2d_filled(na.rm=TRUE, ...))
        temp <- TRUE
      }
      if(tolower(i)=="heat") {
        #out <- out + ggplot2::geom_tile(data=data.2, ggplot2::aes(fill=measurement), na.rm=TRUE, ...)
        d2 <- data[!is.na(data$.value),]
        if(nrow(d2)>0){
            out <- out + suppressWarnings(ggplot2::stat_summary_2d(data=d2, ggplot2::aes(z=.value,...)))
        } else {
          warning("[tubePlot]> a heat map needs a valid z input",
                  call. =FALSE)
        }
        temp <- TRUE
      }
      if(tolower(i)=="boxplot") {
        #out <- out + ggplot2::geom_tile(data=data.2, ggplot2::aes(fill=measurement), na.rm=TRUE, ...)
        out <- out + ggplot2::geom_boxplot(na.rm=TRUE,...)
        temp <- TRUE
      }
      if(tolower(i)=="statribbon") {
        #nb this is killing existing .value
        d2 <- data
        d2$.value <- getTubeX(d2, y)
        d2 <- d2[!is.na(d2$.value),]
        if(nrow(d2)>0){
          d2 <- data.table::as.data.table(d2)
          d2 <- d2[, .(.value=mean(.value, na.rm=TRUE),
                       .value.hi=max(.value, na.rm=TRUE),
                       .value.low=min(.value, na.rm=TRUE)),
                    by =c(x,.xargs$facet, .xargs$group)]
          d2 <- as.data.frame(d2)
          d2 <- d2[!is.na(d2$.value),]
          d2[[y]] <- d2$.value
          out <- out + suppressWarnings(ggplot2::geom_ribbon(data=d2, ggplot2::aes_string(x=x, ymax =".value.hi", ymin=".value.low", group=.xargs$group, ...)))
          out <- out + suppressWarnings(ggplot2::geom_path(data=d2, ggplot2::aes_string(x=x, y=".value", group=.xargs$group, ...)))
        } else {
          warning("[tubePlot]> a heat map needs a valid z input",
                  call. =FALSE)
        }
        temp <- TRUE
      }


      if(!temp){
        test <- c(test, i)
      }
    }
    if(length(test)>0){
      warning("[tubePlot]> did not understand/ignoring: ", paste(test, sep=","),
              call.=FALSE)
    }
    return(out)
  }


#' @rdname tube.plots
#' @export

tubeTimePlot <-
  function(data, x=NULL, y=NULL, plot.type="point",
           avg = NULL,
           ...){

    # setup
    .xargs <- list(...)
    if(length(plot.type)==1){
      plot.type <- unlist(strsplit(plot.type, ","))
    }
    plot.type <- tolower(gsub(" ", "", plot.type))
    data <- tagTubeDate(data) # just passing data at moment

    if(is.null(x) & is.null(y)){
      x <- ".index"
      data$.index <- 1:nrow(data)
      y <- ".date"
    }

    if(is.null(x) & !is.null(y)){
        x=".date"
    } else {
      if(is.null(y)& !is.null(y)){
        y=".date"
      }
    }

    if(!is.null(avg)){
      data$cheat <- data$measurement
      data <- calcTubeStat(data, "cheat", by =c(x, y,
                                                .xargs$facet,
                                                .xargs$group))
      names(data) <- gsub("[.]mean", "", names(data))
    }

    #plot
    out <- ggplotTubeShell(data, x, y, ...)
    #loop through plot options
    for(i in plot.type){
      if(tolower(i)=="point") {
        out <- out + ggplot2::geom_point(na.rm=TRUE)
      }
      if(tolower(i)=="density") {
        out <- out + ggplot2::geom_density_2d_filled(na.rm=TRUE, ...)
      }
      if(tolower(i)=="heat") {
        #out <- out + ggplot2::geom_tile(data=data.2, ggplot2::aes(fill=measurement), na.rm=TRUE, ...)
        out <- out + ggplot2::stat_summary_2d(ggplot2::aes(z=measurement))
      }
      if(tolower(i)=="boxplot") {
        #out <- out + ggplot2::geom_tile(data=data.2, ggplot2::aes(fill=measurement), na.rm=TRUE, ...)
        out <- out + ggplot2::geom_boxplot(na.rm=TRUE,...)
      }
    }
    return(out)

  }




####################################
# ggplotTubeShell
####################################

# workhorse for default plots in testTube... functions

# used by
#   testTubeAccuracy
#   testTubePrecision

#####################
# doing/testing
#####################

# working up new version of ggplotTubeShell
# (currently holding on to previous as ggplotTubeShell_old unexported)

# extended ggplotTubeShell handling to allow col mapping without grouping
# testing/tidying - probably staying...
#                   BUT currently not documented...

# have provisional quicktext variation in
# dte_quickText (unexported and in zzz.r at moment)
# just enabled when auto.text=TRUE in ggplotTubeShell call...
## ggplotTubeShell(iris, "Sepal.Length", "Sepal.Width", xlab="no2", auto.text=TRUE)
# currently only on xlab and ylab
# needs fixing/tidying... - probably staying...
#                           BUT currently not documented...


###########################
# notes
###########################

# don't use is.null(.xargs$facet) to test for facet in ...
# because it partial matches to longer facet... names if facet not there!!!

# same for groups, etc...


#############################
# think about
##############################

# think about an option to kill the legend...
#    maybe do by default if group or col are factors or characters and too large
#       but they add a kill option

# think about extending palette to fill?
#    playing with this...
#    tubePlot2(dont.share::dt.bradford.2, plot.type="site", palette=c("white", "green")) + ggplot2::geom_density_2d_filled(breaks=10^(0:10)


#' @rdname tube.plots
#' @export

ggplotTubeShell <-
  function(data, x=NULL, y=NULL, ...){

    #poor man's quickplot...
    ################################

    #.xargs
    .xargs <- list(...)
    #trusting last rather than first version of any argument
    #   that is duplicated...
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]

    # x and y handling
    # indexing if one missing
    # could also check .xargs for an x or x passed down... ?
    if(is.null(x) & is.null(y)){
      stop("[ggplotTubeShell]> Sorry, need at least one of: x,y \n",
           call.=FALSE)
    }
    if(is.null(y)){
      data$.index <- 1:nrow(data)
      y <- ".index"
    } else {
      if(is.null(x)){
        data$.index <- 1:nrow(data)
        x <- ".index"
      }
    }
    data[, x] <- getTubeX(data, x, if.err = "stop<<ggplotTubeShell>>x")
    data[, y] <- getTubeX(data, y, if.err = "stop<<ggplotTubeShell>>y")

    if(!"xlab" %in% names(.xargs)){
      .xargs$xlab <- x
    }
    if(!"ylab" %in% names(.xargs)){
      .xargs$ylab <- y
    }
    if("auto.text" %in% names(.xargs) && .xargs$auto.text){
      .xargs$xlab <- dte_quickText(.xargs$xlab, TRUE)
      .xargs$ylab <- dte_quickText(.xargs$ylab, TRUE)
    }

    ##########################################
    #these currently ignore anything not expected...
    #########################################
    if("group" %in% names(.xargs)){
      data <- checkTubeData(data, x=.xargs$group, n.x=1,
                            if.err = "stop<<ggplotTubeShell>>group")
      #if unique(group) less than 20 treat as factor...
      if(length(unique(data[[.xargs$group]]))<20){
        data[[.xargs$group]] <- factor(data[[.xargs$group]])
      }
    }
    if("facet" %in% names(.xargs)){
      data <- checkTubeData(data, x=.xargs$facet, n.x=2,
                            if.err = "stop<<ggplotTubeShell>>facet")
    }

    #don't need this is no facet plotting to worry about...
    if(!"facet.type" %in% names(.xargs)){
      .xargs$facet.type <- "wrap"
    } else {
      check <- c("wrap", "grid", "grid.col", "grid.row")
      if(!.xargs$facet.type %in% check){
        warning("bad facet.type resetting to wrap")
        .xargs$facet.type <- "wrap"
      }
    }

    #main ggplot shell build
    plt <- ggplot2::ggplot(data, ggplot2::aes(x=.data[[x]],
                                              y=.data[[y]]))
    #group
    if("group" %in% names(.xargs)){
      plt$mapping$group <- data[[.xargs$group]]
      plt$mapping$colour <- data[[.xargs$group]]
    }

    if("col" %in% names(.xargs)){
      data <- checkTubeData(data, x=.xargs$col, n.x=1,
                            if.err = "stop<<ggplotTubeShell>>col")
      ##this overrides group option if both in call...
      plt$mapping$colour <- data[[.xargs$col]]
    }

    #facet
    if("facet" %in% names(.xargs)){
      ## facet.type choices limited earlier
      #####################
      #tesing
      # passing just ggplot2::facet... args forward
      # names(formals(ggplot2::facet_grid)), etc....
      # if you give it facet, rows or cols
      #     these will override as well
      # does the job but code is messy...
      #####################
      ..xargs <- if(.xargs$facet.type=="wrap"){
        .xargs[names(.xargs) %in% names(formals(ggplot2::facet_wrap))]
      } else {
        #most be a facet_grid
        .xargs[names(.xargs) %in% names(formals(ggplot2::facet_grid))]
      }
      if(length(.xargs$facet)== 1){
        if(.xargs$facet.type=="wrap"){
          ..xargs <- modifyList(list(facets=ggplot2::vars(.data[[.xargs$facet[1]]])),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_wrap, ..xargs)
          #plt <- plt + ggplot2::facet_wrap(facets=ggplot2::vars(.data[[.xargs$facet[1]]]))
        }
        if(.xargs$facet.type %in% c("grid", "grid.row")){
          ..xargs <- modifyList(list(rows=ggplot2::vars(.data[[.xargs$facet[1]]])),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_grid, ..xargs)
          #plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(.data[[.xargs$facet[1]]]))
        }
        if(.xargs$facet.type %in% c("grid.col")){
          ..xargs <- modifyList(list(cols=ggplot2::vars(.data[[.xargs$facet[1]]])),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_grid, ..xargs)
          #plt <- plt + ggplot2::facet_grid(cols=ggplot2::vars(.data[[.xargs$facet[1]]]))
        }
      } else{
        if(.xargs$facet.type %in% c("wrap")){
          ..xargs <- modifyList(list(facets=c(ggplot2::vars(.data[[.xargs$facet[1]]]),
                                              ggplot2::vars(.data[[.xargs$facet[2]]]))),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_wrap, ..xargs)
          #plt <- plt + ggplot2::facet_wrap(facets=c(ggplot2::vars(.data[[.xargs$facet[1]]]),
          #                                 ggplot2::vars(.data[[.xargs$facet[2]]])))
        }
        if(.xargs$facet.type %in% c("grid", "grid.row")){
          plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(.data[[.xargs$facet[1]]]),
                                           cols=ggplot2::vars(.data[[.xargs$facet[2]]]))
        }
        if(.xargs$facet.type %in% c("grid.col")){
          plt <- plt + ggplot2::facet_grid(cols=ggplot2::vars(.data[[.xargs$facet[1]]]),
                                           rows=ggplot2::vars(.data[[.xargs$facet[2]]]))
        }
      }
    }

    #palette
    ############################
    # note
    ############################
    #   testing a colouring option
    ############################
    if("palette" %in% names(.xargs)){
      if(is.null(plt$mapping$colour)){
        plt$mapping$colour <- "default"
        plt <- plt + ggplot2::scale_color_manual(values=.xargs$palette,
                                                 guide="none")
      } else {
        if(is.numeric(plt$mapping$colour)){
          plt <- plt + ggplot2::scale_color_gradientn(colours=.xargs$palette)
        } else {
          if(length(unique(plt$mapping$colour)) > length(.xargs$palette)){
            .xargs$palette <- colorRampPalette(.xargs$palette)(length(unique(plt$mapping$colour)))
          }
          plt <- plt + ggplot2::scale_color_manual(values=.xargs$palette)
        }
      }
    }

    plt <- plt +
      ggplot2::xlab(.xargs$xlab) +
      ggplot2::ylab(.xargs$ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="transparent"))
    if("auto.text" %in% names(.xargs) && .xargs$auto.text){
      ###########################
      #note
      ###########################
      # following may need expanding to other plot labels
      #     strip, scale, etc ???
      plt <- plt +  ggplot2::theme(axis.title.x = ggtext::element_markdown(),
                                   axis.title.y = ggtext::element_markdown())
    }
    return(plt)

  }











###########################
# unexported
##########################



ggplotTubeShell_old <-
  function(data, x=NULL, y=NULL, ...){

    #poor man's quickplot...

    #.xargs
    .xargs <- list(...)
    #trusting last rather than first version of any argument
    #   that is duplicated...
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]

    # x and y handling
    # indexing if one missing
    if(is.null(x) & is.null(y)){
      stop("[ggplotTubeShell]> Sorry, need at least one of: x,y \n",
           call.=FALSE)
    }
    if(is.null(y)){
      data$.index <- 1:nrow(data)
      y <- ".index"
    } else {
      if(is.null(x)){
        data$.index <- 1:nrow(data)
        x <- ".index"
      }
    }
    data[, x] <- getTubeX(data, x, if.err = "stop<<ggplotTubeShell>>x")
    data[, y] <- getTubeX(data, y, if.err = "stop<<ggplotTubeShell>>y")

    if(!"xlab" %in% names(.xargs)){
      .xargs$xlab <- x
    }
    if(!"ylab" %in% names(.xargs)){
      .xargs$ylab <- y
    }

##########################################
#these currently drop anything they cannot
#   evaluate without any error
#########################################
    if("group" %in% names(.xargs)){
      temp <- getTubeX(data, .xargs$group)
      if(is.null(temp)){
        .xargs$group <- NULL
      } else {
#####################
# this makes a factor
# which would be bad if
# user tried to group a big numeric...
#####################
        #data[, .xargs$group] <- getTubeX(data, .xargs$group)
        #if(length(unique(data[, .xargs$group]))<25) {
        #  data[, .xargs$group] <- factor(data[, .xargs$group])
        #}
        data[, .xargs$group] <- getTubeX(data, .xargs$group)
      }
    }
    if("facet" %in% names(.xargs)){
      for(i in 1:length(.xargs$facet)){
        temp <- getTubeX(data, .xargs$facet[i])
        if(is.null(temp)){
          .xargs$facet[i] <- "..bad"
        } else {
          data[, .xargs$facet[i]] <- getTubeX(data, .xargs$facet[i])
        }
      }
      .xargs$facet <- .xargs$facet[.xargs$facet!="..bad"]
      if(length(.xargs$facet)==0) {
        .xargs$facet <- NULL
      }
    }

    if(!"facet.type" %in% names(.xargs)){
      .xargs$facet.type <- "wrap"
    } else {
      check <- c("wrap", "grid", "grid.col", "grid.row")
      if(!.xargs$facet.type %in% check){
        warning("bad facet.type resetting to wrap")
        .xargs$facet.type <- "wrap"
      }
    }

    #main ggplot shell build
    plt <- if(!"group" %in% names(.xargs)) {
      ggplot2::ggplot(data, ggplot2::aes(x=.data[[x]],
                                         y=.data[[y]]))
    } else {
      ggplot2::ggplot(data, ggplot2::aes(x=.data[[x]],
                                         y=.data[[y]],
                                         col=.data[[.xargs$group]],
                                         group=.data[[.xargs$group]]))
    }
    if("facet" %in% names(.xargs)){
      ## facet.type choices limited earlier

#####################
#tesing
# passing just ggplot2::facet... args forward
# names(formals(ggplot2::facet_grid)), etc....
# if you give it facet, rows or cols
#     these will override as well
# does the job but code is messy...
#####################
      ..xargs <- if(.xargs$facet.type=="wrap"){
        .xargs[names(.xargs) %in% names(formals(ggplot2::facet_wrap))]
      } else {
        #most be a facet_grid
        .xargs[names(.xargs) %in% names(formals(ggplot2::facet_grid))]
      }
      if(length(.xargs$facet)== 1){
        if(.xargs$facet.type=="wrap"){
          ..xargs <- modifyList(list(facets=ggplot2::vars(.data[[.xargs$facet[1]]])),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_wrap, ..xargs)
          #plt <- plt + ggplot2::facet_wrap(facets=ggplot2::vars(.data[[.xargs$facet[1]]]))
        }
        if(.xargs$facet.type %in% c("grid", "grid.row")){
          ..xargs <- modifyList(list(rows=ggplot2::vars(.data[[.xargs$facet[1]]])),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_grid, ..xargs)
          #plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(.data[[.xargs$facet[1]]]))
        }
        if(.xargs$facet.type %in% c("grid.col")){
          ..xargs <- modifyList(list(cols=ggplot2::vars(.data[[.xargs$facet[1]]])),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_grid, ..xargs)
          #plt <- plt + ggplot2::facet_grid(cols=ggplot2::vars(.data[[.xargs$facet[1]]]))
        }
      } else{
        if(.xargs$facet.type %in% c("wrap")){
          ..xargs <- modifyList(list(facets=c(ggplot2::vars(.data[[.xargs$facet[1]]]),
                                                     ggplot2::vars(.data[[.xargs$facet[2]]]))),
                                ..xargs)
          plt <- plt + do.call(ggplot2::facet_wrap, ..xargs)
          #plt <- plt + ggplot2::facet_wrap(facets=c(ggplot2::vars(.data[[.xargs$facet[1]]]),
          #                                 ggplot2::vars(.data[[.xargs$facet[2]]])))
        }
        if(.xargs$facet.type %in% c("grid", "grid.row")){
          plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(.data[[.xargs$facet[1]]]),
                                         cols=ggplot2::vars(.data[[.xargs$facet[2]]]))
        }
        if(.xargs$facet.type %in% c("grid.col")){
          plt <- plt + ggplot2::facet_grid(cols=ggplot2::vars(.data[[.xargs$facet[1]]]),
                                           rows=ggplot2::vars(.data[[.xargs$facet[2]]]))
        }

      }
    }
    plt <- plt +
      ggplot2::xlab(.xargs$xlab) +
      ggplot2::ylab(.xargs$ylab) +
      ggplot2::theme_bw()
    return(plt)

  }




