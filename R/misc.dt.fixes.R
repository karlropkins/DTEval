##################################################
#' @title Miscellaneous Diffusion Tube Data Fixes
##################################################

#' @name misc.dt.fixes
#' @aliases misc.dt.fixes
#' @description Miscellaneous code for fixing commonly reported issues with
#' diffusion tube (DT) data.

# general data fix code

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param x The name of a data-series in \code{data} or expression to
#' be evaluated, supplied as a character string. See Details below.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{getTubeX} attempts to extract \code{x} from \code{data} (or
#' build it from information in \code{data}. It uses \code{with(data, x)},
#' so can evaluates terms like:
#'
#' \code{getTubeX(data, 'factor(y)')}
#'
#' (assuming \code{y} is in \code{data} or visible from it, and \code{x}
#' is a character string)
#'
#' It is intended for use with a single \code{x} term and returns it as a
#' vector.
#'
#' \code{checkTubeData} is a general purpose wrapper for \code{getTubeX} that
#' handles multiple \code{x} terms. It attempts to evaluate each in turn and
#' returns the supplied \code{data} plus any additions required.

#' @return \code{getTubeX} returns \code{x} if it can
#' be evaluated with the supplied \code{data}.
#'
#' \code{getTubeData} returns \code{data} plus any additions columns required to
#' allow all valid \code{x} to be used directly as column names, e.g, in form:
#'
#' \code{data[, x]}
#'




#############################
# padTubeMeta
#############################

# might want to check through getTubeX

# notes
# get warning
# Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
# Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
# ℹ Please use `reframe()` instead.
# ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()` always returns an ungrouped data frame and adjust
# accordingly.
# maybe switch to data.table
# but be a big job... because I'll need to go through the package and do the lot...

#' @rdname padTubeMeta
#' @export

padTubeMeta <- function(data, x=NULL, site.id=NULL,...){
  if(is.null(x)){
    return(data)
  }
  if(is.null(site.id)){
    # use tag if there
    if(".site_id" %in% names(data)){
      site.id <- ".site_id"
    } else {
      stop("[padTubeMeta] Sorry, need a valid site.id",
           call.=FALSE)
    }
  }
  #######################
  # fix this and
  # the import in zzz.r
  #########################
  library(data.table)
  .data <- checkTubeData(data, c(x, site.id), if.err="stop")[, c(x,site.id)]
  .data <- data.table::as.data.table(.data)
  .data <- .data[, .(out = na.omit(unique(get(x)))), by=c(site.id)]
  setnames(.data, c("out"), x)
  data <- data[!names(data) %in% x]
  .data <- merge.data.table(data, .data, by=site.id)
  .data <- as.data.frame(.data)
  return(.data)
}

# dt.bradford <- dont.share::dt.bradford
# testTubePrecision(tagTube(dt.bradford))
# dat2 <- padTubeMeta(dt.bradford, "latitude", "site_name")
# dat2 <- padTubeMeta(dat2, "longtitude", "site_name")

# compare
# testTubePrecision(dat3)
# testTubePrecision(dat3)
