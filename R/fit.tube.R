#######################################################
#' @title Fit Diffusion Tube Models
#######################################################

#' @name fit.tube
#' @aliases fit.tube fitTubeModel
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
#'
#' @param model The model to fit, by default:
#'
#' \code{TO DOC}
#' @param simplify Option to simplify the \code{data} before fitting by
#' averaging common inputs, default \code{FALSE}.
#' @param new.data The type of prediction to generate. The default,
#' \code{NULL}, returns the supplied \code{data} with an extra column
#' of the requested predictions added as \code{[tube].pred}. The alternative
#' option \code{'input.ranges'} generates a \code{data.frame} of the model
#' input ranges.
#' @param ... additional arguments, currently passed on to \code{model} and
#' used if recognised.

# I know it is newdata in most model predicts...

#' @details
#' \code{fitTubeModel} attempts to fit a model to the supplied inputs.
#'
#' It and related functions assume that \code{data} is data
#' \code{DTEval} will recognise as Diffusion Tube data, so either
#' previously tagged tube data or data that is tag-able using a
#' default call of \code{\link{tagTube}}

#' @return All functions return a \code{data.frame} of requested
#' \code{data} statistics.



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
#     basic calculation
#         stat can be set as a functions
#


#  thinking about
##############################

# example????
##############################
#   calcTubeStat(dt.gla, by="local_authority")

#   #NB the space trips Site[space]Type... need to ``
#   calcTubeStat(dt.gla, by="`Site Type`")
#   #that messy???

#' @rdname fit.tube
#' @export

fitTubeModel <- function(data, tube = ".value", inputs = NULL,
                         by = NULL, model = NULL, simplify = FALSE,
                         new.data = NULL, ...){

  #setup
  .xargs <- list(...)
  # tag data
  # d2 should have all tags if data is recognisable dt data
  d2 <- tagTube(data)

  # data checks
  d2 <- checkTubeData(d2, tube, if.err="stop<<fitTubeModel>>tube")
  d2 <- checkTubeData(d2, inputs, if.err="stop<<fitTubeModel>>inputs")
  d2 <- checkTubeData(d2, by, if.err="stop<<fitTubeModel>>by")

  # model (method)
  ###############################
  # to do
  ###############################
  if(is.null(model)){
    model <- function(x) { list(mean=mean(x, na.rm=TRUE)) }
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
    d2$..index <- "default"
    by <- "..index"
  }

  #using/adding index as multiple-column catch-all for by
  d2$..index <- as.vector(as.matrix(d2[,by]))

  #new.data
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
      .nd <- do.call(expand.grid, ans)
    }
  } else {
    if(is.data.frame(new.data)){
      #################
      # needs testing
      # needs a right-columns checks
      .nd <- new.data
    }
  }

  #fit model per index (all by's) case
  ans <- lapply(unique(d2$..index), function(x){
    .form <- paste(tube, "~te(", paste(inputs, collapse =","), ")", sep="")
    .form <- as.formula(.form)
    .dd <- d2[d2$..index==x,]
    row.names(.dd) <- 1:nrow(.dd)
    mod <- mgcv::gam(.form, data=.dd)
    if(is.null(new.data)){
      .nd <- .dd
    }
    if(!is.data.frame(.nd)){
      stop("[fitTubeModel] new.data option not understood",
           call. = FALSE)
    }
    #NB: this one is newdata not new.data
    #    because it is predict argument...
    .tmp <- mgcv::predict.gam(mod, newdata=.nd)
    .nd$..pred <- NA
    .nd$..pred[as.numeric(names(.tmp))] <- as.vector(.tmp)
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
      .nd <- .nd[!df.ex,]
    }
    ## too.far only works for 2 inputs ...
    ## could drop predictions of nd
    ##     BUT some plots, etc, might need a full grid...
    ##     could make a too.far(d1, d2, dist=n) to handle any number of columns ??
    .nd
  })
  ans <- do.call(rbind, ans)
  ans <- ans[names(ans) != "..index"]

  #output
  return(ans)
}



