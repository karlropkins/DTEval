#######################################################
#' @title Fit Diffusion Tube Models
#######################################################

#' @name fit.tube
#' @aliases fit.tube fitTubeModel fitTubeModel_gam fitTubeModel_loess
#' @description Functions for fitting diffusion tube (DT)
#' data calculations using \code{DTEval}.

# common calc functions

############################################################
############################################################
# If you are doing a lot of this you should
#       learn r and work directly with the data....
############################################################
############################################################

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param tube The name of the data-series (in \code{data}) to fit a model
#' to (see \code{model}), typically the DT NO2 concentrations (in ug/m3).
#' @param inputs The name(s) of the model inputs.
#' @param by The name(s) of hierarchical grouping terms. These are used to
#' sub-sample the data when build multiple models.
#' @param model The model to fit, by default \code{fitTubeModel_loess}
#' @param simplify Option to simplify the \code{data} before fitting by
#' averaging common inputs, default \code{FALSE}.
#' @param min.count The minimum data count required for a model build.
#' The default, -1, disables this option.
#' @param min.prop The minimum proportion of non-NA data required for a model
#' build. The default, -1, disables this option and values higher than 1
#' exclude all supplied data.
#' @param new.data The type of prediction to generate. The default,
#' \code{NULL}, returns the supplied \code{data} with an extra column
#' of the requested predictions added as \code{[tube].pred}. The alternative
#' option \code{'input.ranges'} generates a \code{data.frame} of the model
#' input ranges.
#' @param ... additional arguments, currently passed on to \code{model} and
#' used if recognised.

# I know it is newdata in most model predicts...
# this currently outputs [tube].pred and [tube].pred.se
#      not sure [tube].pred.se is staying

#' @details
#' \code{fitTubeModel} attempts to fit a model to the supplied inputs.
#'
#' It and related functions assume that \code{data} is data
#' \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return a \code{data.frame} of requested
#' \code{data} statistics.

#' @examples
#' \dontrun{
#' #basic examples
#' mod <- fitTubeModel(dt.brd, inputs=c(".latitude", ".longitude"),
#'                     by =".year", simplify = T)
#' tubePlot(mod, ".value", ".value.pred", facet=".year")
#' }
#'
#'

#############################
# fitTubeModel
#############################

# need to document

# current test
#    dd <- tagTube(dont.share::dt.bradford.2); dd <- dd[dd$.longitude<0,]
#    dd$.year <- format(dd$.start_date, "%Y")
#    a <- fitTubeModel(dd, inputs=c(".latitude", ".longitude"), by =".year", simplify = T)
#    ggplot2::ggplot(a) + ggplot2::geom_point(ggplot2::aes(x=.longitude, y=.latitude, col=.value)) +ggplot2::facet_wrap(.~.year)

# currently does
###############################

#     model fit wrapper
#         model needs to be a function that adds ..pred and ..pred.se to data/new.data


# thinking about
##############################

# currently can't handle non-numeric inputs...
#     so fit inputs = ".date" will die...


# like a krig or similar as an alternative surface for maps...
#     maybe fields (version 1.3-1)
#           Krig: Kriging surface estimate

# other models, e.g. gam spline option like te(x1, x2)
# but how to implement it ???
#     maybe benchtest time for a dedicated gam/spline and
#           a if(switch, tensor, spline) ???


# example????
##############################

# using expand.grid to make new.data = "input.ranges" output
# see
#  https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
#  https://stackoverflow.com/questions/30085487/expand-grid-function-for-data-frames-in-r


#' @rdname fit.tube
#' @export

fitTubeModel <- function(data, tube = ".value", inputs = NULL,
                         by = NULL, model = NULL, simplify = FALSE,
                         min.count = -1, min.prop = -1,
                         new.data = NULL, ...){

  #setup
  .xargs <- list(...)
  # tag data
  # d2 should have all tags if data is recognisable dt data
  d2 <- tagTubeRequired(data, required=c(tube, inputs, by), ...)

  # data checks
  d2 <- checkTubeData(d2, tube, if.err="stop<<fitTubeModel>>tube")
  d2 <- checkTubeData(d2, inputs, if.err="stop<<fitTubeModel>>inputs")
  d2 <- checkTubeData(d2, by, if.err="stop<<fitTubeModel>>by")

  # model (method)
  ###############################
  # to do
  ###############################
  if(is.null(model)){
    model <- fitTubeModel_loess
  }

  #simplify
  if(simplify){
    .stat <- function(x) { list(smooth=mean(x, na.rm=TRUE)) }
    d2 <- suppressWarnings(calcTubeStat(d2, tube, stat=.stat,
                                        by =c(inputs,by)))
    names(d2) <- gsub(".smooth", "", names(d2))
  }

  ## subsetting
  if(is.null(by)){
    d2$..dummy <- "default"
    by <- "..dummy"
  }

  #using/adding index as multiple-column catch-all for by
  d2$..index <- if(length(by)==1){
    d2[, by]
  } else {
    apply(d2[, by], 1, function(x) paste(x, collapse = "*"))
  }

  #new.data
  ###########################
  # why do we need .nd ??
  ###########################
  .nd <- NULL
  if(is.character(new.data)){
    if(new.data=="input.ranges"){
      ans <- lapply(inputs, function(ii){
        .temp <- d2[[ii]]
        #  reset for fit grid.resolution
        .gr <- if("grid.resolution" %in% names(.xargs)){
          .xargs$grid.resolution
        } else {
          100
        }
        .gb <- if("grid.borders" %in% names(.xargs)){
          (max(.temp, na.rm=TRUE) - min(.temp, na.rm=TRUE)) * .xargs$grid.borders
        } else {
          0
        }
        seq(min(.temp, na.rm=TRUE)-.gb, max(.temp, na.rm=TRUE)+.gb, length=.gr)
      })
      names(ans) <- inputs
      ############################
      # next bit messy
      #   just adding KEEP.OUT.ATTRS = FALSE
      #      to the expand.grid arguments
      #      so predict.loess output is vector
      #         (NOT matrix...)
      ############################
      ans[[length(ans)+1]] <- FALSE
      names(ans)[length(ans)] <- "KEEP.OUT.ATTRS"
      .nd <- do.call(expand.grid, ans)
    } else {
      warning("[fitTubeModel] Sorry, did not understand input.range; ignoring...",
              call. = FALSE)
    }
  } else {
    if(is.data.frame(new.data)){
      #################
      # needs testing
      # needs a right-columns checks
      .nd <- new.data
    }
  }
  if(!is.null(.nd) & !is.data.frame(.nd)){
    stop("[fitTubeModel] new.data option not understood",
         call. = FALSE)
  }

  ############################
  # force.positive
  #   not documenting/showing at moment...
  #   not sure we are keeping this...
  .fp <- 1
  if("force.positive" %in% names(.xargs)){
    if(.xargs$force.positive){
      .fp <- 0.5
      d2[[tube]] <- d2[[tube]]^.fp
    }
  }
  #################################

  #fit model per index (all by's) case
  ans <- lapply(unique(d2$..index), function(x){
    #.form <- paste(tube, "~te(", paste(inputs, collapse =","), ")", sep="")
    #.form <- as.formula(.form)
    .dd <- d2[d2$..index==x,]
    ################################
    #test min.count
    #test min.prop
    ################################
    .tst <- .dd[[tube]]
    if(min.count > 0 & length(.tst[!is.na(.tst)]) < min.count){
      return(NULL)
    }
    if(min.prop > 0 & length(.tst[!is.na(.tst)])/length(.tst) < min.prop){
      return(NULL)
    }
    ##############################
    # could try wrap next step
    # and dump a NULL if it fails
    ##############################
    .nd <- try(model(.dd, tube, inputs, .nd),
               silent=TRUE)
    if(inherits(.nd, "try-error")){
      return(NULL)
    }

    ######################################
    # testing for models that can
    #      return NULL
    #####################################
    if(is.null(.nd)){
      return(NULL)
    }

    #row.names(.dd) <- 1:nrow(.dd)
    #mod <- mgcv::gam(.form, data=.dd)
    #if(is.null(new.data)){
    #  .nd <- .dd
    #}
    #if(!is.data.frame(.nd)){
    #  stop("[fitTubeModel] new.data option not understood",
    #       call. = FALSE)
    #}
    #NB: this one is newdata not new.data
    #    because it is predict argument...
    #.tmp <- mgcv::predict.gam(mod, newdata=.nd)
    #.nd$..pred <- NA
    #.nd$..pred[as.numeric(names(.tmp))] <- as.vector(.tmp)
    names(.nd)[names(.nd)=="..pred"] <- paste(tube, ".pred", sep="")
    .nd[c(by, "..index")] <- .dd[1, c(by, "..index")]
    if("too.far" %in% names(.xargs) & !is.null(new.data)){
      df.ex <- if(length(inputs)==2){
        #using mgcv::exclude.too.far because it is faster
        mgcv::exclude.too.far(.nd[, inputs[1]], .nd[, inputs[2]],
                              .dd[, inputs[1]], .dd[, inputs[2]],
                              .xargs$too.far)
      } else {
        #use my MUCH slower generalised version...
        dte_too.far(.nd[inputs], .dd[inputs], .xargs$too.far)
      }
      ################################
      # testing
      ################################
      # was/is
      #.nd <- .nd[!df.ex,]
      # but needs NA colour to be transparent in scale_colour/fill...
      .nd[df.ex, paste(tube, ".pred", sep="")] <- NA
      ################################
    }
    ## too.far only works for 2 inputs ...
    ## could drop predictions of nd
    ##     BUT some plots, etc, might need a full grid...
    ##     could make a too.far(d1, d2, dist=n) to handle any number of columns ??
    .nd
  })
  ans <- do.call(rbind, ans)
  if(is.null(ans)){
    stop("[fitTubeModel] no viable models...",
         call.=FALSE)
  }
  ans <- ans[names(ans) != "..index"]
  ################
  # force positive
  #   not documenting/showing at moment...
  #       1. don't like it
  #       2. can't force positive on non-numeric (e.g. date/time)
  if("force.positive" %in% names(.xargs)){
    if(tube %in% names(ans)){
      ans[[tube]] <- ans[[tube]]^(1/.fp)
    }
    if("..pred" %in% names(ans)){
      ans$..pred <- ans$..pred^(1/.fp)
    }
  }
  ########################
  # ..pred and ..pred.se
  # should these be checked for ??
  # should we allow user reset
  # should we allow rename
  names(ans)[names(ans)=="..pred"] <- paste(tube, ".pred", sep="")
  names(ans)[names(ans)=="..pred.se"] <- paste(tube, ".pred.se", sep="")
  ###############
  #output
  return(ans)
}



#################################
# models
#################################

#' @rdname fit.tube
#' @export

fitTubeModel_gam <- function(data, tube, inputs, new.data = NULL, ...){

  ################################
  # dropped and work straight from data ??
  #################################
  #d <- data[c(tube, inputs)]
  ###############################
  # might want s(x1) + s(x2)...
  #     instead of te(x1, x2, ...) ???
  # would be
  #.form <- paste(tube, "~s(", paste(inputs, collapse = ")+s(", sep=""), ")", sep="")
  ###############################
  # do we want to pass on args using '...' ???
  ################################
  .form <- paste(tube, "~te(", paste(inputs, collapse =","), ")", sep="")
  .form <- as.formula(.form)
  row.names(data) <- 1:nrow(data)
  ###############################
  mod <- mgcv::gam(.form, data=data)
  if(is.null(new.data)){
    new.data <- data
  }
  ########################
  # should not need ???
  ########################
  if(!is.data.frame(new.data)){
    stop("[fitTubeModel] new.data option not understood",
         call. = FALSE)
  }
  .tmp <- mgcv::predict.gam(mod, newdata=new.data, se.fit=TRUE)
  new.data$..pred <- NA
  new.data$..pred[as.numeric(names(.tmp$fit))] <- as.vector(.tmp$fit)
  new.data$..pred.se <- NA
  new.data$..pred.se[as.numeric(names(.tmp$se.fit))] <- as.vector(.tmp$se.fit)
  ########################
  # doing this in main function
  # so just once
  #names(new.data)[names(new.data)=="..pred"] <- paste(tube, ".pred", sep="")
  #########################
  new.data
}




#' @rdname fit.tube
#' @export

fitTubeModel_loess <- function(data, tube, inputs, new.data = NULL, ...){

  .form <- paste(tube, "~", paste(inputs, collapse ="*"), sep="")
  .form <- as.formula(.form)
  row.names(data) <- 1:nrow(data)
  ###############################
  # loess(y ~ x1 * x2 * etc, surface = "direct", ...)
  #      (think this allows 4 x-terms ???)
  #      (might be a memory killer...)
  ###############################
  mod <- loess(.form, data=data, surface="direct")
  if(is.null(new.data)){
    new.data <- data
  }
  ########################
  # should not need ???
  ########################
  if(!is.data.frame(new.data)){
    stop("[fitTubeModel] new.data option not understood",
         call. = FALSE)
  }
  #new.data <- as.data.frame(as.table(new.data))
  .tmp <- predict(mod, newdata=new.data, se=TRUE)
  new.data$..pred <- NA
  new.data$..pred[as.numeric(names(.tmp$fit))] <- as.vector(.tmp$fit)
  new.data$..pred.se <- NA
  new.data$..pred.se[as.numeric(names(.tmp$se.fit))] <- as.vector(.tmp$se.fit)
  ########################
  # wondering if these should be ..fit not ..pred
  #######################
  #names(new.data)[names(new.data)=="..pred"] <- paste(tube, ".pred", sep="")
  new.data
}

