##################################################
#' @title Miscellaneous Diffusion Tube Data Handling
##################################################

#' @name misc.dt.handlers
#' @aliases misc.dt.handlers getTubeX checkTubeData
#' @description Miscellaneous code for routine diffusion tube (DT) data
#' handling with \code{DTEval}.

# general data handling code

#' @param data Data source, typically a data.frame or similar, containing
#' data-series of diffusion tube records.
#' @param x The name of a data-series in \code{data} or expression to
#' be evaluated, supplied as a character string. See Details below.
#' @param ... additional arguments, currently ignored.
#' @param test.class If supplied, the required class of \code{x} as a
#' character string.
#' @param if.err The action to take if the function fails: By default,
#'  \code{'return.null'} to return a \code{NULL} silently, but other
#' options include \code{'stop'} to stop as \code{getTubeX},
#' and \code{'stop<<FUN>>ARG'} to stop with function 'FUN' reporting a
#' problem with argument ARG.
#' @param n.x The maximum number of \code{x} that to be checked.
#' @param output Where different outputs are an option, the requested
#' output, e.g. data or report.

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
# getTubeX
#############################

## think about
## a <- "Species"
## a <- "Species == iris$Species[1]"
## etc...
## with(iris, eval(parse(text=a)))

#' @rdname misc.dt.handlers
#' @export

getTubeX <- function(data, x=NULL, ..., test.class=NULL,
                     if.err="return.null"){

  # blame
  fun.nm <- "[getTubeX]"
  x.nm <- "x"
  if(grepl("^stop", if.err) & grepl("<<", if.err) & grepl(">>", if.err)){
    test <- gsub("^stop|^stop<<|>>.*", "", if.err)
    if(test != ""){
      fun.nm <- paste("[", test, "]", sep="")
    }
    test <- gsub(".*>>", "", if.err)
    if(test != ""){
      x.nm <- test
    }
  }

  # only 1 x term...
  if(length(x)!=1){
    if(if.err=="return.null"){
      return(NULL)
    } else {
      if(length(x) <1){
        stop(fun.nm, " no ", x.nm, "...",
             call.=FALSE)
      } else {
        stop(fun.nm, " Sorry, only 1 ", x.nm, " term allowed",
             call.=FALSE)
      }
    }
  }

  # evaluate
  out <- try(with(data, eval(parse(text=x))),
             silent=TRUE)
  if(class(out)[1]=="try-error"){
    out <- try(data[, x],
               silent = TRUE)
  }
  if(class(out)[1]=="try-error") {
    if(if.err=="return.null"){
      return(NULL)
    }
    if(grep("^stop", if.err)){
      stop(fun.nm, " Sorry, can't find/build ", x.nm, " term '", x,
           "'\n", call.=FALSE)
    }
    #back to default
    return(NULL)
  } else {
    if(!is.null(test.class)){
      test <- try(is(out, test.class))
      if((class(test)[1]=="try-error") || !test){
        if(if.err=="return.null"){
          return(NULL)
        } else {
          stop(fun.nm, " ", x.nm, " not expected ", test.class, " class\n",
               call.=FALSE)
        }
      }
    }
    out
  }
}


#############################
# checkTubeData
#############################

## wrapper for getTubeX
## returning x's to data

#' @rdname misc.dt.handlers
#' @export

checkTubeData <- function(data, x=NULL, ..., n.x=-1, if.err="return.null",
                          output = "data"){

  if(is.null(x)){
    return(data)
  }
  # blame
  fun.nm <- "[checkTubeData]"
  x.nms <- "x"
  if(grepl("^stop", if.err) & grepl("<<", if.err) & grepl(">>", if.err)){
    test <- gsub("^stop|^stop<<|>>.*", "", if.err)
    if(test != ""){
      fun.nm <- paste("[", test, "]", sep="")
    }
    test <- gsub(".*>>", "", if.err)
    if(test != ""){
      x.nms <- test
    }
  }

  if(n.x<0 | length(x) <= n.x){
    for(i in 1:length(x)){
      temp <- getTubeX(data, x[i], if.err=if.err)
      if(is.null(temp)){
        #holding in case we want a warning...
        x[i] <- "..bad"
      } else {
        data[, x[i]] <- temp
      }
    }
  } else {
    stop(fun.nm, " Sorry, only ", n.x, " ", x.nms, " term(s) allowed",
         call.=FALSE)
  }

  if(output=="data"){
    return(data)
  }
  if(output=="report"){
    return(x[x %in% names(data)])
  }
  warning(fun.nm, " unknown output; check ?CheckTubeData",
          call. = FALSE)
  return(NULL)
}








#################
# like getTubeData
# probably not keeping
###################

#dte_sneak <- function(){
#  temp <- as.list(sys.call(-1))
#  out <- as.character(sys.call(-1))
#  names(out) <- names(temp)
#  return(out)
#
#  b <- try(eval(parse(text=a)))
#  if(!class(b)[1]=="try-error") {
#    return(b)
#  }
#  b <- try(with(data, eval(parse(text=a))))
#  if(!class(b)[1]=="try-error") {
#    return(b)
#  }
#
#  #b <- try(eval(substitute(a)))
#  #if(!class(b)[1]=="try-error") {
#  #  return(b)
#  #}
#  b <- try(with(data, eval(substitute(a))))
#  if(!class(b)[1]=="try-error") {
#    return(b)
#  }
#
#  return(NULL)
#}


#dte_get <- function(x, xxx, data=NULL){
#  #out <- as.character(xxx[x])
#  #return(out)
#  if(is.null(data)){
#    out <- as.character(xxx[x])
#    if(out=="NULL"){
#      return(NULL)
#    } else {
#      return(as.character(out))
#    }
#  }
#  out <- try(with(data, eval(parse(text=as.character(xxx[x])))),
#             silent=TRUE)
#  if(class(out)[1]=="try-error"){
#    NULL
#  } else {
#    out
#  }
#}






