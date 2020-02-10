#' Information-theoretic meta-features
#'
#' Information-theoretic meta-features are particularly appropriate to describe
#' discrete (categorical) attributes, but they also fit continuous ones so a
#' discretization is required.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param features A list of features names or \code{"all"} to include all them.
#'  The supported values are described in the details section. (Default: 
#'  \code{"all"})
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param transform A logical value indicating if the numeric attributes should 
#'  be transformed. If \code{FALSE} they will be ignored. (Default: 
#'  \code{TRUE})
#' @param ... Further arguments passed to the summarization functions.
#' @details 
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"attrConc"}{Attributes concentration. It is the Goodman and 
#'      Kruskal's tau measure otherwise known as the concentration coefficient
#'      computed for each pair of attributes (multi-valued).}
#'    \item{"attrEnt"}{Attributes entropy, a measure of randomness of each 
#'      attributes in the dataset (multi-valued).}
#'  }
#'  This method uses the unsupervised data discretization procedure provided by
#'  \link[infotheo]{discretize} function, where the default values are used when
#'  \code{transform=TRUE}.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Donald Michie, David J. Spiegelhalter, Charles C. Taylor, and John Campbell. 
#'  Machine Learning, Neural and Statistical Classification, volume 37. Ellis 
#'  Horwood Upper Saddle River, 1994.
#'
#'  Alexandros Kalousis and Melanie Hilario. Model selection via meta-learning: 
#'  a comparative study. International Journal on Artificial Intelligence Tools,
#'  volume 10, pages 525 - 554, 2001.
#'
#'  Ciro Castiello, Giovanna Castellano, and Anna Maria Fanelli. Meta-data: 
#'  Characterization of input features for meta-learning. In 2nd International 
#'  Conference on Modeling Decisions for Artificial Intelligence (MDAI), 
#'  pages 457 - 468, 2005.
#'
#' @examples
#' ## Extract all metafeatures
#' infotheo(iris[,1:4])
#'
#' ## Extract some metafeatures
#' infotheo(iris[1:4], c("attrConc", "attrEnt"))
#'
#' ## Extract all meta-features without summarize the results
#' infotheo(iris[1:4], summary=c())
#'
#' ## Use another summarization functions
#' infotheo(iris[1:4], summary=c("min", "median", "max"))
#' 
#' ## Do not transform the data (using only categorical attributes)
#' infotheo(iris[1:4], transform=FALSE)
#' @export
infotheo <- function(...) {
  UseMethod("infotheo")
}

#' @rdname infotheo
#' @export
infotheo <- function(...) {
  UseMethod("infotheo")
}

#' @rdname infotheo
#' @export
infotheo.default <- function(x, features="all", 
                                summary=c("mean", "sd"),
                                transform=TRUE, ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(features[1] == "all") {
    features <- ls.infotheo()
  }
  features <- match.arg(features, ls.infotheo(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
  if(length(summary) == 0) {
    summary <- "non.aggregated"
  }
  
  if(transform) {
    x <- categorize(x)
  }

  extra <- list(
    x.entropy = sapply(x, entropy)
  )

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, list(x=x, extra=extra))
    post.processing(measure, summary, f %in% ls.infotheo.multiples(), ...)
  }, simplify=FALSE)
}

#' List the information theoretical meta-features
#'
#' @return A list of information theoretical meta-features names
#' @export
#'
#' @examples
#' ls.infotheo()
ls.infotheo <- function() {
  c("attrConc", "attrEnt")
}

ls.infotheo.multiples <- function() {
  c("attrConc", "attrEnt")
}

m.attrConc <- function(x, ...) {
  if (ncol(x) == 1) return(NA)
  comb <- expand.grid(i=seq(ncol(x)), j=seq(ncol(x)))
  comb <- comb[comb$i != comb$j, ]

  mapply(function(i, j) {
    concentration.coefficient(x[, i], x[, j])
  }, i=comb$i, j=comb$j)
}

m.attrEnt <- function(extra, ...) {
  extra$x.entropy
}
