############################################
#' @title Diffusion Tube Data Plots
############################################

#' @name tube.plots
#' @aliases tube.plots tubePlot tubeTimePlot ggplotTubeShell
#' @description \code{DTEval} uses \code{ggplot2} to generate most plots.
#'
#' Obviously, you are welcome to use graphics package you know to generate
#' you own plot. In fact, if you want really fine control of plot outputs, we
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
#' @param overplot.action Optional data handling for over-plotting. The default,
#' \code{NULL}, allows over-plotting of co-located points. The current
#' alternative, enabled by all \code{overplot.action} settings, is to (mean)
#' average co-located point records.
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


#' @return \code{tubePlot} is a general purpose plot for diffusion tube
#' data.
#'
#' \code{ggplotTubeShell} returns a ggplot object the user then
#' needs to add \code{geom}s to, to build a standard plot.
#'
#' @note Maybe I'm already missing \code{qplot}...



# need to
#########################

# reference and link to ggplot2... and/or plotting more generally in R

# decide and document col/colour handling

#


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

# re-built ggplotTubeShell to better handle mapping/aes terms

# proposing making tubePlot the standard plot
# then tubeTimePlot a variation that by default sets x  to date
# also think about making tubeSitePlot tubeMap without the map...

#####################
# notes
#####################

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
           overplot.action = NULL,
           ...){

    # might want to run all the .xargs that are data
    # through checkTubeData ???

    # setup
    .xargs <- list(...)
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]
    if(!is.null(.xargs$facet)){
      data$..facet <- getTubeX(data, .xargs$facet)
      .xargs$facet <- "..facet"
    }
    if(is.null(x) & is.null(y)){
      stop("[tubePlot]> need at least one of 'x' and 'y'...",
           call.=FALSE)
    }
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
    # this needs careful handling...
    # only for plot.type point and similar
    if(!is.null(overplot.action)){
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
      data <- suppressWarnings(calcTubeStat(data, .x, by =.by))
      names(data) <- gsub("[.]mean", "", names(data))
    }

    ##################################
    # main plot
    ##################################

    # stuff we are passing to ggplotTubeShell
    .ggshell <- c("facet", "facet.type", "xlab", "ylab",
                  "auto.text", "palette")
    .safe <- .xargs[names(.xargs) %in% .ggshell]
    .safe <- modifyList(list(data=data, x=x, y=y), .safe)
    out <- do.call(ggplotTubeShell, .safe)
    # remove stuff handled by ggplotTubeShell
    .xargs <- .xargs[!names(.xargs) %in% .ggshell]

    #################################
    # add plot layers
    #################################

    .unknown <-c()
    for (i in plot.type){
      temp <- FALSE
      if(i=="point"){
        #standard point
        temp <- TRUE
        out <- dte_ggshellAddGeom(.xargs, data, out,
                                 ggplot2::geom_point,
                                 defaults = list(na.rm=TRUE),
                                 drops = c())
      }
      if(i=="ghost"){
        #standard point
        temp <- TRUE
        d2 <- data[names(data) != "..facet"]
        out <- dte_ggshellAddGeom(.xargs, d2, out,
                                 ggplot2::geom_point,
                                 defaults = list(na.rm=TRUE),
                                 holds = list(colour = "grey"))
      }

      if(i=="count"){
        #standard count - might not stay
        temp <- TRUE
        out <- dte_ggshellAddGeom(.xargs, data, out,
                                 ggplot2::geom_count, list(na.rm=TRUE),
                                 c())
      }
      if(!temp){
        .unknown <- c(.unknown, i)
      }
    }
    #    print(.unknown)
    if(length(.unknown)>0){
      warning("[tubePlot]> did not understand/ignoring plot.type : ",
              paste(.unknown, sep=",", collapse=","),
              call.=FALSE)
    }
    return(out)
  }






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
    .xargs.test <- dte_ggshellTestArgs(.xargs, data)

    ##    print(.xargs.test)

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
    if("group" %in% names(.xargs)){
      #plt$mapping$group <- rlang::parse_quo(.xargs$group, env=environment())
      plt$mapping$group <- getTubeX(data, .xargs$group)
      if(!"col" %in% names(.xargs)){
        #plt$mapping$colour <- rlang::parse_quo(.xargs$group, env=environment())
        plt$mapping$colour <- getTubeX(data, .xargs$group)
      }
    }

    if("col" %in% names(.xargs)){
      if(.xargs.test$col=="data"){
        #plt$mapping$colour <- rlang::parse_quo(.xargs$col, env=environment())
        plt$mapping$colour <- getTubeX(data, .xargs$col)
      }
    }
    if("size" %in% names(.xargs)){
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
      # plt$mapp$.. are not quosures...
      if(is.null(plt$mapping$colour)){
        plt$mapping$colour <- "default"
        plt <- plt + ggplot2::scale_color_manual(values=.xargs$palette,
                                                 guide="none")
      } else {
        .pp <- data[[rlang::as_label(plt$mapping$colour)]]
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





















#############################
# unexported and going ???
#############################



tubePlot_old  <-
  function(data, x=NULL, y=NULL, plot.type="point",
           overplot.action = NULL,
           ...){

    # setup
    .xargs <- list(...)
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]
    if(!is.null(.xargs$facet)){
      data$..facet <- getTubeX(data, .xargs$facet)
      .xargs$facet <- "..facet"
    }
    print(.xargs)
    if(is.null(x) & is.null(y)){
      stop("[tubePlot]> need at least one of 'x' and 'y'...",
           call.=FALSE)
    }
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
    # this needs careful handling...
    # only for plot.type point and similar
    if(!is.null(overplot.action)){
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
#print(.x)
#print(.by)
      data <- suppressWarnings(calcTubeStat(data, .x, by =.by))
      names(data) <- gsub("[.]mean", "", names(data))
    }




    ##################################
    # main plot
    ##################################

    ..xargs <- modifyList(list(data=data, x=x, y=y), .xargs)
    out <- do.call(ggplotTubeShell, ..xargs)

    # loop through plot.type
    .unknown <-c()
    for (i in plot.type){
      temp <- FALSE
      if(i=="point"){
        #standard point
        temp <- TRUE
        out <- out + ggplot2::geom_point(na.rm=TRUE)
        .tt <- .xargs.test[.xargs.test!="data"]
        #print(.tt)
        # could automate this
        #    BUT it will be a lot easier to break...
        #    ALSO some stuff here need special handling
        #          col info goes to colour
        #          na.rm is a geom (not aes) argument
        #          it may drop ggplot error messaging...
        .n.layer <- length(out$layers)
        if(length(.tt)>0){
          for(i in 1:length(.tt)){
            if(names(.tt)[i]=="col"){
              out$layers[.n.layer][[1]]$aes_params$colour <- ..xargs[[names(.tt)[i]]]
            }
            if(names(.tt)[i]=="alpha"){
              out$layers[.n.layer][[1]]$aes_params$alpha <- ..xargs[[names(.tt)[i]]]
            }
            if(names(.tt)[i]=="size"){
              out$layers[.n.layer][[1]]$aes_params$size <- ..xargs[[names(.tt)[i]]]
            }
            if(names(.tt)[i]=="na.rm"){
              out$layers[.n.layer][[1]]$geom_params$na.rm <- ..xargs[[names(.tt)[i]]]
            }
          }
        }
      }
      if(i=="ghost"){
        #ghost points
        temp <- TRUE
        d2 <- data[!names(data) %in% .xargs$facet]
        #d2 <- data
        out <- out + ggplot2::geom_point(data=d2,
                                         col="grey", na.rm=TRUE)
        print(names(data))
        print(names(d2))

        .tt <- .xargs.test[.xargs.test!="data"]
        # like above
        .n.layer <- length(out$layers)
        if(length(.tt)>0){
          for(i in 1:length(.tt)){
            #this would track something like ghost.col
            #if(names(.tt)[i]=="col"){
            #  out$layers[.n.layer][[1]]$aes_params$colour <- ..xargs[[names(.tt)[i]]]
            #}
            if(names(.tt)[i]=="alpha"){
              out$layers[.n.layer][[1]]$aes_params$alpha <- ..xargs[[names(.tt)[i]]]
            }
            if(names(.tt)[i]=="size"){
              out$layers[.n.layer][[1]]$aes_params$size <- ..xargs[[names(.tt)[i]]]
            }
            if(names(.tt)[i]=="na.rm"){
              out$layers[.n.layer][[1]]$geom_params$na.rm <- ..xargs[[names(.tt)[i]]]
            }
          }
        }
      }
      if(!temp){
        .unknown <- c(.unknown, i)
      }
    }
#    print(.unknown)
    if(length(.unknown)>0){
      warning("[tubePlot]> did not understand/ignoring plot.type : ", paste(.unknown, sep=",", collapse=","),
              call.=FALSE)
    }
    return(out)


    ##################################
    ##################################


    #loop through plot options
    test <-c()
    for(i in plot.type){
      temp <- FALSE
      if(tolower(i)=="point") {
        out <- out + ggplot2::geom_point(na.rm=TRUE)
        temp <- TRUE
      }
      if(tolower(i)=="density") {
        out <- out + ggplot2::geom_density_2d_filled(na.rm=TRUE,...)
        temp <- TRUE
      }
      if(tolower(i)=="heat") {
        #out <- out + ggplot2::geom_tile(data=data.2, ggplot2::aes(fill=measurement), na.rm=TRUE, ...)
        d2 <- data[!is.na(data$.value),]
        if(nrow(d2)>0){
            test <- dt_geom_shell("ggplot2::geom_point", d2, .xargs,
                                 default=list(na.rm=TRUE), ignore=c("facet", "palette"))
            print(test)
            out <- out + ggplot2::stat_summary_2d(data=d2,
                                                  do.call(function() ggplot2::aes(z=.value, x), test$aes))
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
      if(tolower(i)=="stat.ribbon") {
        #nb this is killing existing .value
        d2 <- data
        d2$.value <- getTubeX(d2, y)
        d2 <- d2[!is.na(d2$.value),]
        if(nrow(d2)>0){
          d2 <- data.table::as.data.table(d2)
          d2 <- d2[, .(.value=mean(.value, na.rm=TRUE),
                       .value.hi=max(.value, na.rm=TRUE),
                       .value.low=min(.value, na.rm=TRUE)),
                    by =c(x,.xargs$facet, .xargs$group,
                          .xargs$col)]
          d2 <- as.data.frame(d2)
          d2 <- d2[!is.na(d2$.value),]
          ..xargs <- modifyList(.xargs, list(x={{x}}, y={{".value"}},
                                            ymax ={{".value.hi"}}, ymin={{".value.low"}}))
          tt <- dt_geom_shell("ick", d2, ..xargs, ignore="facet")
          print(tt$aes)

          if(length(tt$aes)>0){
            for(i in 1:length(tt$aes)){
              print(names(tt$aes)[i])
              if(tt$aes[[i]] %in% names(d2)){
                print(tt$aes[[i]])
                print(names(tt$aes)[i])
                ..xargs[[names(tt$aes)[i]]] <- d2[[tt$aes[[i]]]]
              }
            }
          }
          if("group" %in% names(..xargs) & !"col" %in% names(..xargs)){
            ..xargs$col <- ..xargs$group
          }
          tt <- dt_geom_shell("ick", d2, ..xargs)
          ##print(..xargs)

          #d2[[y]] <- d2$.value
          out <- out + ggplot2::geom_ribbon(data=d2, do.call(ggplot2::aes, ..xargs), alpha=0.1, linetype=0)
          ..xargs$ymax <- NULL
          ..xargs$ymin <- NULL
          out <- out + ggplot2::geom_path(data=d2, do.call(ggplot2::aes, ..xargs))
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
      warning("[tubePlot]> did not understand/ignoring: ", paste(test, collaspe=","),
              call.=FALSE)
    }
    return(out)
  }


#dt_geom_shell <- function(geom.name, data, .xargs, default=NULL,
#                          ignore=NULL, use=NULL){
#  if(is.null(.xargs)){
#    .xargs <- list()
#  }
#  if(!is.null(default)){
#    .xargs <- modifyList(default, .xargs)
#  }
#  .l1 <- .xargs[as.vector(.xargs) %in% names(data)]
#  .l2 <- .xargs[!as.vector(.xargs) %in% names(data)]
#  .tmp <- .l1[unlist(lapply(.l1, is.numeric))]
#  .l1 <- .l1[!names(.l1) %in% names(.tmp)]
#  .l2 <- modifyList(.l2, .tmp)
#  if(!is.null(ignore)){
#    .l1 <- .l1[!names(.l1) %in% ignore]
#    .l2 <- .l2[!names(.l2) %in% ignore]
#  }
#  if(!is.null(use)){
#    .l1 <- .l1[names(.l1) %in% ignore]
#    .l2 <- .l2[names(.l2) %in% ignore]
#  }
#  return(list(aes=.l1, rest=.l2))
#  #build geom call as string
#  out <- paste(geom.name, "(", sep="")
#  if(length(.l1)>0){
#    out <- paste(out, "ggplot2::aes(", sep="")
#    out <- paste(out,
#                 paste(names(.l1), "=.data[[", as.vector(.l1), "]]",
#                       sep="", collapse = ","),
#                 ")",
#                 sep="")
#  }
#  if(length(.l1)>0 & length(.l2)>0){
#    out <- paste(out, ",", sep="")
#  }
#  if(length(.l2)>0){
#    out <- paste(out,
#                 paste(names(.l2), "=", as.vector(.l2), "",
#                       sep="", collapse = ","),
#                 sep="")
#  }
#  out <- paste(out, ")", sep="")
#  out
#
#
#  #list(.xargs=.xargs, .l1 = .l1, .l2 = .l2, .tmp=.tmp)
#
#}


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










###########################
# unexported
##########################


ggplotTubeShell_old <-
  function(data, x=NULL, y=NULL, ...){

    #poor man's quickplot...
    ################################

    #.xargs
    .xargs <- list(...)
    #trusting last rather than first version of any argument
    #   that is duplicated...
    .xargs <- .xargs[!duplicated(names(.xargs), fromLast=TRUE)]
    .xargs.test <- dte_ggShellTest(.xargs, data)

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
        # plt$mapping$colour is quosure..
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







