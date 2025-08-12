############################################
#' @title Diffusion Tube ggplot
############################################

#' @name tube.plots
#' @aliases tube.plots tubePlot ggplotTubeShell
#' @description \code{DTEval} uses ggplot2 to generate most plots.
#' \code{ggplotTubeShell} builds common plot shells for many of these, and
#' handles some of of the generic plot control. \code{tubePlot} is a
#' wrapper for several commonly used plots. Functions
#' like \code{\link{testTubePrecision}} and \code{\link{testTubePrecision}}
#' also use it to generate  associated plot outputs.
#' @param data Data source, typically a data.frame or similar, to be used to
#' build a plot using ggplot2.
#' @param x,y The names of the data-series to plot on the
#'  X and Y axes, respectively.
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


#############################
# notes
#############################

# removed full imports
## #' @import dplyr, etc
## now specifying directly in code, ggplot2::ggplot, etc

# don't use is.null(.xargs$facet) to test for facet in ...
# because it partial matches to longer facet... names if facet not there!!!

# same for groups, etc...



####################################
# plotTube
####################################

# common plots warpper


#####################
# first draft
#####################

# maybe use ggplotTubeShell for this???

# options for boxplot, point, smooth ect geom type
# x,y defaults, e.g. x = .date for a time plot
# time averaging, etc...
# commonly used plots



#' @rdname tube.plots
#' @export

tubePlot <-
  function(data, x=NULL, y=NULL, ...){

    out <- ggplotTubeShell(data, x, y, ...) +
      ggplot2::geom_smooth()
    out

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




