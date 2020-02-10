#' General meta-features
#'
#' General meta-features include general information related to the dataset. It
#' is also known as simple measures.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param features A list of features names or \code{"all"} to include all them.
#'  The supported values are described in the details section. (Default: 
#'  \code{"all"})
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' 
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"attrToInst"}{Ratio of the number of attributes per the number of 
#'    instances, also known as dimensionality.}
#'    \item{"catToNum"}{Ratio of the number of categorical attributes per the 
#'    number of numeric attributes.}
#'    \item{"instToAttr"}{Ratio of the number of instances per the number of 
#'    attributes.}
#'    \item{"nrAttr"}{Number of attributes.}
#'    \item{"nrBin"}{Number of binary attributes.}
#'    \item{"nrCat"}{Number of categorical attributes.}
#'    \item{"nrInst"}{Number of instances.}
#'    \item{"nrNum"}{Number of numeric attributes.}
#'    \item{"numToCat"}{Ratio of the number of numeric attributes per the number
#'    of categorical attributes.}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Donald Michie, David J. Spiegelhalter, Charles C. Taylor, and John Campbell.
#'  Machine Learning, Neural and Statistical Classification, volume 37. Ellis 
#'  Horwood Upper Saddle River, 1994.
#'
#'  Guido Lindner and Rudi Studer. AST: Support for algorithm selection with a 
#'  CBR approach. In European Conference on Principles of Data Mining and 
#'  Knowledge Discovery (PKDD), pages 418 - 423, 1999.
#'
#'  Ciro Castiello, Giovanna Castellano, and Anna M. Fanelli. Meta-data: 
#'  Characterization of input features for meta-learning. In 2nd International 
#'  Conference on Modeling Decisions for Artificial Intelligence (MDAI), 
#'  pages 457 - 468, 2005.
#'
#' @examples
#' ## Extract all metafeatures
#' general(iris[,1:4])
#'
#' ## Extract some metafeatures
#' general(iris[1:100, 1:4], c("nrAttr", "nrClass"))
#' 
#' ## Extract all meta-features without summarize prop.class
#' general(iris[,1:4], summary=c())
#' 
#' ## Use another summarization functions
#' general(iris[,1:4], summary=c("sd","min","iqr"))
#' @export
general <- function(...) {
  UseMethod("general")
}

#' @rdname general
#' @export
general.default <- function(x, features="all",
                               summary=c("mean", "sd"), 
                               ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(features[1] == "all") {
    features <- ls.general()
  }
  features <- match.arg(features, ls.general(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
  if(length(summary) == 0) {
    summary <- "non.aggregated"
  }

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, list(x=x))
    post.processing(measure, summary, f %in% ls.general.multiples(), ...)
  }, simplify=FALSE)
}

#' List the general meta-features
#'
#' @return A list of general meta-features names
#' @export
#'
#' @examples
#' ls.general()
ls.general <- function() {
  c("attrToInst", "catToNum", "instToAttr", "nrAttr", "nrBin", 
    "nrCat", "nrInst", "nrNum",  "numToCat")
}

ls.general.multiples <- function() {
  c()
}

#Meta-features
m.attrToInst <- function(x, ...) {
  m.nrAttr(x) / m.nrInst(x)
}

m.catToNum <- function(x, ...) {
  nnum <- m.nrNum(x)
  if (nnum == 0) return(NA)
  m.nrCat(x) / nnum
}

m.instToAttr <- function(x, ...) {
  m.nrInst(x) / m.nrAttr(x)
}

m.nrAttr <- function(x, ...) {
  ncol(x)
}

m.nrBin <- function(x, ...) {
  sum(apply(x, 2, function (col) length(table(col)) == 2))
}

m.nrCat <- function(x, ...) {
  m.nrAttr(x) - m.nrNum(x)
}

m.nrInst <- function(x, ...) {
  nrow(x)
}

m.nrNum <- function(x, ...) {
  sum(sapply(x, is.numeric))
}

m.numToCat <- function(x, ...) {
  ncat <- m.nrCat(x)
  if (ncat == 0) return(NA)
  m.nrNum(x) / ncat
}
