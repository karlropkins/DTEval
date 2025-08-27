############################################
#' @title Diffusion Tube Data Plots
############################################

#' @name tube.plots
#' @aliases tube.plots tubePlot tubeTimePlot ggplotTubeShell
#' @description \code{DTEval} uses \code{ggplot2} to generate most plots.
#'
#' Obviously, you are welcome to use any graphics package you know to generate
#' your own plot. In fact, if you want really fine control of plot outputs, we
#' recommend taking the time to learn a graphics package like
#' \code{ggplot}, \code{lattice}, etc. But \code{DTEval} also includes
#' several quick-use wrappers for some commonly used plot types and useful
#' options like grouping and faceting, which can really help with data
#' visualisation. It also includes \code{ggplotTubeShell}, which builds
#' common plot shells for many of these, and handles some of of
#' the generic plot control for functions like \code{\link{testTubePrecision}}
#' and \code{\link{testTubePrecision}}.

# maybe link to ggplot2 and a r help page on graphics for this?

#' @param data Data source, typically a data.frame or similar, to be used to
#' build a plot using ggplot2.
#' @param x,y The names of the data-series to plot on the
#'  X and Y axes, respectively, and assumed to be elements of \code{data}.
#' @param plot.type The type of plot required.
#' @param ... Additional arguments. See details below.

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
#'
#'  Functions like \code{tubePlot} add layers to this framework to
#'  generate common plots.
#'

## see this about best practice using ggplot2 in a package...
##    (their words not mine..)
## https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html


#' @return \code{tubePlot} is a general purpose plotting functions for
#' diffusion tube data.
#'
#' \code{tubeTimePlot} is a \code{tubePlot} wrapper that adds the tagged
#' sampling date as a second axes if one of \code{x} or \code{y} is
#' supplied.
#'
#' \code{ggplotTubeShell} returns a ggplot object the user then
#' needs to add \code{geom}s to, to build a standard plot.
#'
#' @note Maybe I'm already missing \code{qplot}...



###############################
# documentation notes
###############################

# need to
#######################

#    reference and link to ggplot2... and/or plotting more generally in R

# think about
#######################

# decide and document col/colour handling

# decide and document palette and fill.palette handling



#############################
# code notes - general
#############################

# removed full imports
## #' @import dplyr, etc
## now specifying directly in code, ggplot2::ggplot, etc


# most urgent
############################

# possible issue with point.count when it is same as one of the two axes...
#    see
#      tubeTimePlot(tagTube(dont.share::dt.bradford.2), y="measurement", plot.type = "point.count", size="measurement")
#    and what I think it should look like...
#       tubeTimePlot(tagTube(dont.share::dt.bradford.2), y="measurement", plot.type = "point.count", size=".latitude")




# think about
##############################

# think about an option to kill the legend...
#    maybe do by default if group or col are factors or characters and too large
#       but they add a kill option

# option rotate axes labels

# control of scale free, free_x, free_y, etc...

# think about extending palette to fill?
#    [might now be done... via ggplotTUbeShell?]
#    playing with this...
#    tubePlot(dont.share::dt.bradford.2, plot.type="site", palette=c("white", "green")) + ggplot2::geom_density_2d_filled(breaks=10^(0:10)

# think about an option to pass argument to just one layer
#    can obviously do that in a function...
#        but not from a tubePlot call...

# like something like smooth but not as talkative...

#



####################################
# plotTube
####################################

# common plots wrapper

########################
# second draft ???
########################

# re-built ggplotTubeShell to better handle mapping/aes terms

# proposing making tubePlot the standard plot
# then tubeTimePlot a variation that by default sets x  to date
# also think about making tubeSitePlot tubeMap without the map...

#####################
# notes
#####################

# NEED to go through these notes....
#    (some might be done/fixed now...)

# using ggplotTubeShell
#     dte_ggshellTestArgs
#     dte_ggshellAddGeom
#     getTubeX and checkTubeData

# once we have range of required plots agreed
#     we should be able to simplify/automate a lot of this

# playing with

# nice visualizations....
#    https://www.nature.com/articles/s41467-018-03297-7

# see this about irregular data
#    https://stackoverflow.com/questions/37529116/how-to-plot-a-heat-map-with-irregular-data-in-ordinates-in-ggplot

# also
#    tubePlot(dont.share::dt.bradford.2, plot.type="site", palette=c("white", "green")) + ggplot2::geom_density_2d_filled(breaks=10^(0:10)) +ggplot2::xlim(-2.5,-1) + ggplot2::ylim(53.7, 54)






# looking at
# tubePlot(dt.york, x=".start_date", y="measurement", plot.type="statribbon", palette="white")
# not tracking groups ...
# tubePlot(dt.york, x=".start_date", y="measurement", plot.type="statribbon", group="month", col="month")
# this sort of works...
# tubePlot(dt.york, x=".start_date", y="measurement", plot.type="statribbon", group="month", col="month")

# saw this on over.plotting
# https://bookdown.dongzhuoer.com/hadley/ggplot2-book/overplotting




#' @rdname tube.plots
#' @export


tubePlot <-
  function(data, x=NULL, y=NULL, plot.type="point",
           ...){

    # might want to tidy this once methods are finalised

    # setup
    .xargs <- list(...)
    #what to do about col/colour...?
    #   currently using this cludge in both tubePlot and ggplotTubeShell
    #      and assuming color hereafter...
    names(.xargs)[names(.xargs)=="col"] <- "colour"
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]

    #need this if facet source is also used for other args, e.g. col...
    if(!is.null(.xargs$facet)){
      data$..facet <- getTubeX(data, .xargs$facet)
      .xargs$facet <- "..facet"
    }

    # x/y handling
    ####################################
    # do we want .index option handling...
    ####################################
    if(is.null(x) & is.null(y)){
      stop("[tubePlot]> need at least one of 'x' and 'y'...",
           call.=FALSE)
    }
    if(is.null(x)){
      x <- ".default"
      data$.default <- ""
      if(!"xlab" %in% names(.xargs)){
        .xargs$xlab <- ""
      }
    }
    if(is.null(y)){
      y <- ".default"
      data$.default <- ""
      if(!"ylab" %in% names(.xargs)){
        .xargs$ylab <- ""
      }
    }
    data[, x] <- getTubeX(data, x, if.err = "stop<<ggplotTubeShell>>x")
    data[, y] <- getTubeX(data, y, if.err = "stop<<ggplotTubeShell>>y")

    .xargs.test <- dte_ggshellTestArgs(.xargs, data)
    .dd <- .xargs[names(.xargs) %in% names(.xargs.test[.xargs.test=="data"])]
    data <- checkTubeData(data, unlist(.dd))
    .xargs.test <- dte_ggshellTestArgs(.xargs, data)

    # main args (x,y,group, facet) are checked
    #     BUT ALSO might want to run all .xargs
    #         identified as data through checkTubeData ???
    #         to allow stuff like col = "factor(measurement)", etc... ???

    if(length(plot.type)==1){
      plot.type <- unlist(strsplit(plot.type, ","))
    }
    plot.type <- tolower(gsub(" ", "", plot.type))

    ################################
    # overplot.action
    ################################
    # moving calculation into layers where it is used
    # BUT calculating parameters early because they could be used multiple times...
    #    so calculating .by and .x for using in calcTubeStat
    #       data <- suppressWarnings(calcTubeStat(data, .x, by =.by))
    #    AND
    #       stat <- function(x) { list(mean=mean(x, na.rm=TRUE))
    # SEE point.mean as example...

    #if(!is.null(overplot.action)){
      .by <- c(x, y, .xargs$facet, .xargs$group)
      .x <- c()     ##### temp setting
      if(length(.xargs)>0){
        for(i in 1:length(.xargs)){
          #print(.xargs[[i]]) - value
          #print(names(.xargs)[i]) - name
          if(.xargs.test[[i]]=="data"){
            if(!is.numeric(getTubeX(data, .xargs[[i]]))){
              .by <- c(.by, .xargs[[i]])
            } else {
              .x <- c(.x, .xargs[[i]])
            }
          }
        }
      }
      .by <- unique(.by)
      .x <- unique(.x[!.x %in% .by])
      if(length(.x)<1){
        #add dummy
        data$..dummy <- 1
        .x="..dummy"
      }
    #  names(data) <- gsub("[.]mean", "", names(data))
    #}

    ##################################
    # main plot
    ##################################
    #  should be able to tidy this once range of plots agreed...

    # stuff ggplotTubeShell deals with...
    .ggshell <- c("facet", "facet.type", "xlab", "ylab",
                  "auto.text", "palette", "fill.palette", "map.args")
    #.safe <- .xargs[names(.xargs) %in% c(.ggshell, "colour", "group")]
    #.safe <- modifyList(list(data=data, x=x, y=y), .safe)
    .all <- modifyList(list(data=data, x=x, y=y,
                            map.args = c("x", "y")), .xargs)
    out <- do.call(ggplotTubeShell, .all)
    # remove stuff handled by ggplotTubeShell
    .xargs <- .xargs[!names(.xargs) %in% .ggshell]
    # putting x and y back in, in case we have to kill one of the mappings...
    #    plot.type="hist" does this...
    .xargs <- modifyList(list(x=x, y=y),
                         .xargs)

    #################################
    # add plot layers
    #################################

    .unknown <-c()
    for (i in plot.type){
      temp <- FALSE
      # template
      # if(i==...){
      #   temp <- TEMP
      #   if we change data make d2
      #   if we chande .xargs make .xargs2
      #       or you'll get both all other any follow-on layers...
      # }

      if(i=="box"){
        # box and whisker plot
        #   largely untested
        # does this need a group term
        #    if neither x or y are factors ???
        #       see tubeTimePlot(tagTube(dont.share::dt.bradford.2), y="measurement", plot.type = "box", group=".date")
        # could be boxplot args I don't know about
        #     or allow via dte_ggshellAddGeom
        temp <- TRUE
        drops <-  names(.xargs)[!names(.xargs) %in% dte_GeomArgs(ggplot2::GeomBoxplot)]
        out <- dte_ggshellAddGeom(.xargs, data, out,
                                  ggplot2::geom_boxplot,
                                  defaults = list(na.rm=TRUE),
                                  drops = drops)
      }

      if(i=="hist"){
        # histogram
        # have to kill one of x or y for this
        #    so it does not play nicely with plots that do at the moment...
        # need to allow statistical args
        #    ideally do better than drops below
        # if mapped aesthetics are numerics may need to group or make factor
        #    see
        #        tubePlot(dt.york, x="measurement", plot.type = "hist",
        #                 facet="month", fill="factor(CalendarYear)",
        #                 group="CalendarYear")
        # y axes name should be count...
        #     but might have to cheat to fix that...

        temp <- TRUE
        if(!".default" %in% c(x, y)){
          stop("[tubePlot] sorry; plot.type 'hist' only allows one of x or y...",
               call. = FALSE)
        }
        .xargs2 <- .xargs
        if(x==".default"){
          out$mapping$x <- NULL
          .xargs2$x <- NULL
        }
        if(y==".default"){
          out$mapping$y <- NULL
          .xargs2$y <- NULL
        }
        drops <- names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomBar)]
        drops <- drops[!drops %in% c("bins", "binwidth")]
        out <- dte_ggshellAddGeom(.xargs2, data, out,
                                  ggplot2::geom_histogram,
                                  defaults = list(na.rm=TRUE, bins=30),
                                  drops = drops)
      }


      if(i %in% c("point", "point.mean", "point.count")){
        temp <- TRUE
        if(i == "point.mean"){
          .stat <- function(x) { list(mean=mean(x, na.rm=TRUE)) }
          d2 <- suppressWarnings(calcTubeStat(data, .x, stat=.stat, by =.by))
          #names(d2) <- gsub("[.]mean", "", names(d2))
          .xargs2 <- .xargs
          .xargs2[.xargs2 %in% .x] <- paste(.xargs2[.xargs2 %in% .x], ".mean", sep="")
          drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPoint)]
          out <- dte_ggshellAddGeom(.xargs2, d2, out,
                                    ggplot2::geom_point,
                                    defaults = list(na.rm=TRUE),
                                    drops = drops)

        }
        if(i == "point.count"){
          .stat <- function(x) { list(count=length(x[!is.na(x)])) }
          d2 <- suppressWarnings(calcTubeStat(data, .x, stat=.stat, by =.by))
          .xargs2 <- .xargs
          .xargs2[.xargs2 %in% .x] <- paste(.xargs2[.xargs2 %in% .x], ".count", sep="")
          drops <-  names(.xargs2)[!names(.xargs2) %in% dte_GeomArgs(ggplot2::GeomPoint)]
          out <- dte_ggshellAddGeom(.xargs2, d2, out,
                                    ggplot2::geom_point,
                                    defaults = list(na.rm=TRUE),
                                    drops = drops)
        }
        if(i=="point"){
        drops <-  names(.xargs)[!names(.xargs) %in% dte_GeomArgs(ggplot2::GeomPoint)]
        out <- dte_ggshellAddGeom(.xargs, data, out,
                                 ggplot2::geom_point,
                                 defaults = list(na.rm=TRUE),
                                 drops = drops)
        }
      }

#      ###########################
#      # ghost needs more work if
#      #    averaging is used...
#      if(i=="ghost"){
#      #  #standard point
#        temp <- TRUE
#        d2 <- data[names(data) != "..facet"]
#        out <- dte_ggshellAddGeom(.xargs, d2, out,
#                                 ggplot2::geom_point,
#                                 defaults = list(na.rm=TRUE),
#                                 holds = list(colour = "grey"))
#      }

#      if(i=="bin2d"){
#        #standard count - might not stay
#        temp <- TRUE
#        out <- dte_ggshellAddGeom(.xargs, data, out,
#                                  ggplot2::geom_bin2d, list(na.rm=TRUE),
#                                  c())
#      }
#      if(i=="hex"){
#        #standard count - might not stay
#        temp <- TRUE
#        out <- dte_ggshellAddGeom(.xargs, data, out,
#                                  ggplot2::geom_hex, list(na.rm=TRUE),
#                                  c())
#      }

      if(!temp){
        .unknown <- c(.unknown, i)
      }
    }

    # what was ignored
    if(length(.unknown)>0){
      warning("[tubePlot]> did not understand/ignoring plot.type : ",
              paste(.unknown, sep=",", collapse=","),
              call.=FALSE)
    }

    # returned plot
    return(out)
  }




####################################
# tubeTimePlot
####################################

# tubePlot wrapper that adds .date was other axes if one already set...

# to time about
###################################

# default x/ylab if x/y set ?

#' @rdname tube.plots
#' @export


tubeTimePlot <-
  function(data, x=NULL, y=NULL, plot.type="point",
           ...){

    data <- tagTubeDate(data, ...)
    if(is.null(x) & !is.null(y)){
      x <- ".date"
    }
    if(is.null(y) & !is.null(x)){
      y <- ".date"
    }
    tubePlot(data = data, x = x, y = y,
             plot.type = plot.type, ...)

}




#' @rdname tube.plots
#' @export



ggplotTubeShell <-
  function(data, x=NULL, y=NULL, ...){

    #poor man's quickplot...
    ################################

    #.xargs
    .xargs <- list(...)
    #what to do about this...?
    #using in tubePlot and ggplotTubeShell
    names(.xargs)[names(.xargs)=="col"] <- "colour"
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

    .xargs.test <- dte_ggshellTestArgs(.xargs, data)
    map.args <- if("map.args" %in% names(.xargs)){
      .xargs$map.args
    } else {
      names(.xargs.test[.xargs.test=="data"])
    }

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
        data[[.xargs$group]] <- factor(data[[.xargs$group]], ordered=TRUE)
      }
    }
    if("facet" %in% names(.xargs)){
      data <- checkTubeData(data, x=.xargs$facet, n.x=2,
                            if.err = "stop<<ggplotTubeShell>>facet")
    }

    #don't need this if no facet plotting to worry about...
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

    #######################################################################
    # to think about ....
    #    what is being mapped....
    #        could automate this
    #             BUT some are not standard
    #                   col => colour
    #                   might want to make following optional?
    #                        col = group if group there and col is
    #    the not-mapped stuff needs to go into the layers ...
    #         that needs to be handled post-layer-addition ...
    #             maybe a ggplotShellLocalReset()
    #                with defaults and forced options...

    #group
    if("group" %in% map.args){
      #plt$mapping$group <- rlang::parse_quo(.xargs$group, env=environment())
      plt$mapping$group <- getTubeX(data, .xargs$group)
######################
# this might be better
# in the local plot code for
# the testTube... functions
######################
      if(!"colour" %in% map.args & !"colour" %in% names(.xargs)){
        #plt$mapping$colour <- rlang::parse_quo(.xargs$group, env=environment())
        plt$mapping$colour <- getTubeX(data, .xargs$group)
      }
    }

    if("colour" %in% map.args & "colour" %in% names(.xargs)){
      if(.xargs.test$colour=="data"){
        #plt$mapping$colour <- rlang::parse_quo(.xargs$col, env=environment())
        plt$mapping$colour <- getTubeX(data, .xargs$colour)
      }
    }
    if("size" %in% map.args & "size" %in% names(.xargs)){
      if(.xargs.test$size=="data"){
        #plt$mapping$size <- rlang::parse_quo(.xargs$size, env=environment())
        plt$mapping$size <- getTubeX(data, .xargs$size)
      }
    }

    #######################################################################

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
      # colours might not be being mapped
      if(!"colour" %in% names(.xargs.test[.xargs.test=="data"])){
        plt$mapping$colour <- "default"
        plt <- plt + ggplot2::scale_color_manual(values=.xargs$palette,
                                                 guide="none")
      } else {
        #.pp <- data[[rlang::as_label(plt$mapping$colour)]]
        #.pp <- plt$mapping$colour
        .pp <- getTubeX(data, .xargs$colour)
        if(is.numeric(.pp)){
          plt <- plt + ggplot2::scale_color_gradientn(colours=.xargs$palette)
        } else {
          if(length(unique(.pp)) > length(.xargs$palette)){
            .xargs$palette <- colorRampPalette(.xargs$palette)(length(unique(.pp)))
          }
          plt <- plt + ggplot2::scale_color_manual(values=.xargs$palette)
        }
      }
    }

    #fill.palette
    ############################
    # note
    ############################
    #   testing a colouring option
    ############################
    if("palette" %in% names(.xargs) & ! "fill.palette" %in% names(.xargs)){
      .xargs$fill.palette <- .xargs$palette
    }
    if("fill.palette" %in% names(.xargs)){
      # like colours might not be being mapped
      if(!"fill" %in% names(.xargs.test[.xargs.test=="data"])){
        plt$mapping$fill <- "default"
        plt <- plt + ggplot2::scale_fill_manual(values=.xargs$fill.palette,
                                                 guide="none")
      } else {
        #.pp <- data[[rlang::as_label(plt$mapping$colour)]]
        #.pp <- plt$mapping$colour
        .pp <- getTubeX(data, .xargs$fill)
        if(is.numeric(.pp)){
          plt <- plt + ggplot2::scale_fill_gradientn(colours=.xargs$fill.palette)
        } else {
          if(length(unique(.pp)) > length(.xargs$fill.palette)){
            .xargs$fill.palette <- colorRampPalette(.xargs$fill.palette)(length(unique(.pp)))
          }
          plt <- plt + ggplot2::scale_fill_manual(values=.xargs$fill.palette)
        }
      }
    }


#########################
#force legend together if multiple are mapped
#messy.... should only be if mapped to the same arg...
########################
    if(length(names(.xargs)[names(.xargs) %in% c("colour", "size", "fill")]) >1){
      if("colour" %in% names(.xargs)){
        plt <- plt + ggplot2::guides(colour=ggplot2::guide_legend())
      }
      if("size" %in% names(.xargs)){
        plt <- plt + ggplot2::guides(size=ggplot2::guide_legend())
      }
      if("fill" %in% names(.xargs)){
        plt <- plt + ggplot2::guides(fill=ggplot2::guide_legend())
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

