##################################################
#' @title Diffusion Tube Meta Data
##################################################

#' @name test.tube.meta
#' @aliases tube.meta testTubeMeta
#' @description Testing diffusion tube meta-data. Coded methods are all
#' currently based on the visualisation of meta-data-like behaviour.

# testTubeMeta plot

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param x (Optional) The name of one or more data-series in \code{data} or
#' expressions to be evaluated, supplied as character strings. If not supplied,
#' this defaults to all data-series in \code{code}
#' @param by (Optional) The name of one or more data-series or tags to be
#' used as a case identifier for meta information. If not supplied, this
#' defaults to the \code{all.data}, \code{sample}, \code{location} and
#' \code{date} properties of the data set tags. See Details below.
#' @param ... additional arguments, currently ignored.

#' @details
#' \code{testTubeMeta} generates a plot of meta-like behaviour. It is
#' currently in-development but is intended to be used in the form:
#'
#' \code{testTubeMeta(dt.data)}
#'
#' \code{testTubeMeta} attempts to identify data-series that look like meta
#' information in the supplied \code{data}. By its nature, meta-data
#' is expected to unique at a specific level of aggregation. For example,
#' all samples collected at one site should have the same \code{latitude},
#' \code{longitude}, sample type, etc regardless of their date of collection.
#' By default, \code{testTubeMeta} looks for data-series that are meta-like
#' at four levels of aggregation: \code{all.data} indicating that all records
#' are identical, \code{sample} indicating that all records for a given
#' \code{location} and \code{date} combination are identical; \code{location}
#' indicating that all records for a given \code{location} are identical; and
#' \code{date} indicating that all records for a given \code{date} are
#' identical. Any of these that score 100 percentage indicate meta-like
#' behaviour at the associated level of aggregation. Although, a meta-like does
#' not prove a data-series is meta-data, less than 100 percent scores for a
#' data-series that is expected to be meta-data, would indicate some issues
#' with that data-series.
#'
#' Although the function
#' default is to look for known sample identifiers, this grouping term
#' is set using the addition argument \code{by}. The function then extracts
#' all data-series with only one unique non-\code{NA} value. By default,
#' the function test all non-grouping data-series, but similarly
#' testing/extraction can be limited to specific cases using \code{x}.
#' Please remeber that this function extracts data-series that at the
#' \code{by} grouping-level looks like meta information.
#'
#' @return \code{testTubeMeta} generates a plot of meta-like behaviour.
#' It is a standard \code{ggplot2} output that can be modified using
#' standard \code{ggplot2} methods.
#'
#' @seealso [checkTubeMeta()] and related functions for working with and
#' fixing Diffusion Tube meta-data.

# check about link to checkTubeMeta works...


######################################
# to think about
######################################



#############################
# testTubeMeta
#############################

# might want to check through getTubeX/checkTubeData

# notes
#############################

#     have undocumented plot.type=2
#        document if keeping
#            grey = just NAs
#            green = all identical and NOT NAs
#            amber = all either identical or NA
#            red = multiple terms either with NAs removed
#            (grey and red need manual fixing; and possible provider input)
#            (green look sense/probably fine - usual meta-data assessment caveat)
#            (grey we can probably fix )

#     included a palette arg in .xargs BUT should
#        think about rewriting to use ggplotShell, etc...


#' @rdname tube.meta
#' @export

testTubeMeta <- function(data, x=NULL, by=NULL, ...){

  #setup
  ############################
  # think about the tagging behaviour
  # what do we need/want
  #    and what should we be showing in plot... ?
  .xargs <- modifyList(list(plot.type = 1), list(...))
  .d <- tagTube(data)
  if(is.null(x)){
    x <- names(.d)
  }
  ####################################
  #by
  ####################################
  #   to do
  #   once sample/location/date sorted
  ####################################

  # add temp versions of all, sample, location, date...
  #   currently need these BUT don't keep them
  .d$..location <- tagTubeLocation(.d)$.location
  .d$..date <- tagTubeDate(.d)$.date
  .d$..sample <- .d$.sample_id
  .d$..all <- 1

  if(.xargs$plot.type==1){

    # standard plot
    ######################
    loc.fun <- function(x){
      if(length(unique(x))==1){
        as.integer(length(x[!is.na(x)]))
      } else {
        as.integer(0)
      }
    }
    #cols for plot.type 1
    plt.cols <- if("palette" %in% names(.xargs)){
      rep(.xargs$palette, 4)[1:4]
    } else {
      # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
      # gg_color_hue <- function(n) {
      #     hues = seq(15, 375, length = n + 1)
      #     hcl(h = hues, l = 65, c = 100)[1:n]}
      c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    }
    #build levels
    .test <- calcTubeStat(.d, x, "..all", loc.fun)
    .test$ref<- "all.data"
    .test <- .test[names(.test)!="..all"]
    out <- data.table::as.data.table(.test)
    .test <- calcTubeStat(.d, x, "..sample", loc.fun)
    .test$ref<- "sample"
    out <- data.table::rbindlist(list(out, .test[names(.test)!="..sample"]),
                                 fill=TRUE)
    .test <- calcTubeStat(.d, x, "..date", loc.fun)
    .test$ref<- "date"
    out <- data.table::rbindlist(list(out, .test[names(.test)!="..date"]),
                                 fill=TRUE)
    .test <- calcTubeStat(.d, x, "..location", loc.fun)
    .test$ref<- "location"
    out <- data.table::rbindlist(list(out, .test[names(.test)!="..location"]),
                                 fill=TRUE)
    stat <- function(x) sum(x, na.rm=TRUE)
    tube <- names(out)[names(out) != "ref"]
    out <- out[, as.list(unlist(lapply(.SD, stat))), .SDcols = tube,
               by = "ref"]

    # summary
    out <- as.data.frame(data.table::melt(out, id.vars="ref"))
    out$value <- out$value/nrow(.d) * 100
    out$ref <- factor(out$ref, levels=rev(c("all.data", "sample",
                                            "location", "date")))

    # plot
    # flip legend list order?
    plt <- ggplot2::ggplot(out) +
      ggplot2::geom_col(ggplot2::aes(x=value, y=ref, fill=ref),
                        position="dodge") +
      ggplot2::geom_vline(xintercept = 100, linetype="dashed") +
      ggplot2::facet_wrap(.~variable) +
      ggplot2::scale_fill_manual(name="",
                                 values=c("all.data" = plt.cols[1],
                                          "sample"  = plt.cols[2],
                                          "location" = plt.cols[3],
                                          "date" = plt.cols[4]),
                                 drop = FALSE) +
      ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill=NA))

    return(plt)
  } else {

    # alternative plot
    #################################
    # as above but making two column
    loc.fun <- function(x){
      list(count = length(x),
           type = if(length(unique(x))==1){
                if(is.na(unique(x))) "grey" else "green"
             } else {
                if(length(unique(x[!is.na(x)]))==1) "amber" else "red"
             }
          )
    }
    #cols for plot.type 2
    plt.cols <- if("palette" %in% names(.xargs)){
      rep(.xargs$palette, 4)[1:4]
    } else {
      # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
      # gg_color_hue <- function(n) {
      #     hues = seq(15, 375, length = n + 1)
      #     hcl(h = hues, l = 65, c = 100)[1:n]}
      c(grey(0.85), "green", "lightyellow", "pink")
    }
    .test <- calcTubeStat(.d, x, "..all", loc.fun)
    .test$ref<- "all.data"
    .test <- .test[names(.test)!="..all"]
    out <- data.table::as.data.table(.test)
    .test <- calcTubeStat(.d, x, "..sample", loc.fun)
    .test$ref<- "sample"
    out <- data.table::rbindlist(list(out, .test[names(.test)!="..sample"]),
                                 fill=TRUE)
    .test <- calcTubeStat(.d, x, "..date", loc.fun)
    .test$ref<- "date"
    out <- data.table::rbindlist(list(out, .test[names(.test)!="..date"]),
                                 fill=TRUE)
    .test <- calcTubeStat(.d, x, "..location", loc.fun)
    .test$ref<- "location"
    .dt <- data.table::rbindlist(list(out, .test[names(.test)!="..location"]),
                                 fill=TRUE)
    .nm <- c(names(.dt)[grep("[.]count$", names(.dt))], "ref")
    .test <- .dt[,.nm, with=FALSE]
    names(.test) <- gsub("[.]count$", "", names(.test))
    out <- as.data.frame(data.table::melt(.test, id.vars="ref"))
    .nm <- c(names(.dt)[grep("[.]type$", names(.dt))], "ref")
    .test <- .dt[,.nm, with=FALSE]
    names(.test) <- gsub("[.]type$", "", names(.test))
    .test <- as.data.frame(data.table::melt(.test, id.vars="ref"))
    out$value <- as.numeric(out$value)/nrow(.d) * 100
    out$..type <- factor(.test$value, levels= c("red", "amber", "green", "grey"))
    out$ref <- factor(out$ref, levels=rev(c("all.data", "sample",
                                            "location", "date")))
    #####################
    # might be worth
    #    summing in data.frame and send smaller data.frame to ggplot???
    #        could also retry the transparent red, amber and grey bands
    #        if this speed thinks up...

    plt <- ggplot2::ggplot(out) +
      ggplot2::geom_col(ggplot2::aes(x=value, y=ref, fill=..type),
                        position="stack", show.legend = TRUE) +
      ggplot2::geom_vline(xintercept = 100, linetype="dashed") +
      ggplot2::facet_wrap(.~variable) +
      ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::scale_fill_manual(name="",
                                 values=c("red" = plt.cols[4],
                                          "amber"  = plt.cols[3],
                                          "green" = plt.cols[2],
                                          "grey" = plt.cols[1]),
                                 drop = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill=NA))

    return(plt)

  }
}






