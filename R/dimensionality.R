#' Measures of dimensionality
#'
#' These measures give an indicative of data sparsity. They capture how sparse 
#' a datasets tend to have regions of low density. These regions are know to be 
#' more difficult to extract good classification and regression models.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param ... Not used.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"T2"}{Average number of points per dimension (T2) is given by the 
#'      ratio between the number of examples and dimensionality of the dataset.}
#'    \item{"T3"}{Average number of points per PCA (T3) is similar to T2, but 
#'      uses the number of PCA components needed to represent 95% of data 
#'      variability as the base of data sparsity assessment.}
#'    \item{"T4"}{Ratio of the PCA Dimension to the Original (T4) estimates the
#'      proportion of relevant and the original dimensions for a dataset.}
#'  }
#' @return A list named by the requested dimensionality measure.
#'
#' @references
#'  Ana C Lorena, Ivan G Costa, Newton Spolaor and Marcilio C P Souto. (2012). 
#'    Analysis of complexity indices for classification problems: Cancer gene 
#'    expression data. Neurocomputing 75, 1, 33--42.
#'
#' @examples
#' ## Extract all dimensionality measures
#' dimensionality(iris[,1:4])
#' @export
dimensionality <- function(...) {
  UseMethod("dimensionality")
}

#' @rdname dimensionality
#' @export
dimensionality <- function(...) {
  UseMethod("dimensionality")
}

#' @rdname dimensionality
#' @export
dimensionality.default <- function(x, features="all", ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(features[1] == "all") {
    features <- ls.dimensionality()
  }

  features <- match.arg(features, ls.dimensionality(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)

  x <- binarize(x)

  sapply(features, function(f) {
    eval(call(paste("c", f, sep="."), x=x))
  }, simplify=FALSE)
}

ls.dimensionality <- function() {
  c("T2", "T3", "T4")
}

pca <- function(x) {
  aux <- stats::prcomp(x)
  tmp <- length(which(summary(aux)$importance[3,] < 0.95)) + 1
  return(tmp)
}

c.T2 <- function(x) {
  ncol(x)/nrow(x)
}

c.T3 <- function(x) {
  pca(x)/nrow(x)
}

c.T4 <- function(x) {
  pca(x)/ncol(x)
}