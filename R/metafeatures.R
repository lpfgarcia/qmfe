#' Extract meta-features from a dataset
#'
#' This is a simple way to extract the meta-features from a dataset, where all
#' meta-features from each group is extracted.
#'
#' @param x A data.frame contained only the input attributes.
#' @param groups A list of meta-features groups, \code{"default"} for traditional
#'  groups of meta-features or \code{"all"} to include all them. The details 
#'  section describes the valid values for this parameter.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Optional arguments to the summary methods.
#' @details
#'  The following groups are allowed for this method:
#'  \describe{
#'    \item{"infotheo"}{Include all information theoretical meta-features. See
#'      \link{infotheo} for more details.}
#'    \item{"general"}{Include all general (simple) meta-features. See
#'      \link{general} for more details.}
#'    \item{"statistical"}{Include all statistical meta-features. See
#'      \link{statistical} for more details.}
#'    \item{"dimensionality"}{The dimensionality measures compute information on
#'      how smoothly the examples are distributed within the attributes. See 
#'      \link{dimensionality} for more details.}
#'    \item{"itemset"}{Include all itemset meta-features. See
#'      \link{itemset} for more details.}
#'  }
#'
#' @return A numeric vector named by the meta-features from the specified 
#' groups.
#' @export
#'
#' @examples
#' ## Extract all meta-features
#' metafeatures(iris[,1:4])
#'
#' ## Extract some groups of meta-features
#' metafeatures(iris[1:4], c("general", "statistical", "infotheo"))
#'
#' ## Use another summary methods
#' metafeatures(iris[1:4], summary=c("min", "median", "max"))
metafeatures <- function(...) {
  UseMethod("metafeatures")
}

#' @rdname metafeatures
#' @export
metafeatures.default <- function(x, groups="all",
                                    summary=c("mean", "sd"),
                                    ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(groups[1] == "all") {
    groups <- ls.metafeatures()
  }
  groups <- match.arg(groups, ls.metafeatures(), TRUE)
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  unlist(sapply(groups, function(group) {
    do.call(paste(group, "default", sep='.'), list(x=x, summary=summary, ...))
  }, simplify = FALSE))
}

#' List the meta-features groups
#'
#' @return A list of meta-features groups
#' @export
#'
#' @examples
#' ls.metafeatures()
ls.metafeatures <- function() {
  c("general", "statistical", "infotheo", "dimensionality", "itemset")
}
