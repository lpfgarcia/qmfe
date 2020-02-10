#' Statistical meta-features
#'
#' Statistical meta-features are the standard statistical measures to describe
#' the numerical properties of a distribution of data. As it requires only
#' numerical attributes, the categorical data are transformed to numerical.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param features A list of features names or \code{"all"} to include all them.
#'  The details section describes the valid values for this group.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param transform A logical value indicating if the categorical attributes
#'  should be transformed. If \code{FALSE} they will be ignored. (Default: 
#'  \code{TRUE})
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"cor"}{Absolute attributes correlation, which measure the 
#'    correlation between each pair of the numeric attributes in the dataset 
#'    (multi-valued). This measure accepts an extra argument called 
#'    \code{method = c("pearson", "kendall", "spearman")}. See 
#'    \code{\link[stats]{cor}} for more details.}
#'    \item{"cov"}{Absolute attributes covariance, which measure the covariance 
#'    between each pair of the numeric attributes in the dataset 
#'    (multi-valued).}
#'    \item{"eigenvalues"}{Eigenvalues of the covariance matrix (multi-valued).}
#'    \item{"gMean"}{Geometric mean of attributes (multi-valued).}
#'    \item{"hMean"}{Harmonic mean of attributes (multi-valued).}
#'    \item{"iqRange"}{Interquartile range of attributes (multi-valued).}
#'    \item{"kurtosis"}{Kurtosis of attributes (multi-valued).}
#'    \item{"mad"}{Median absolute deviation of attributes (multi-valued).}
#'    \item{"max"}{Maximum value of attributes (multi-valued).}
#'    \item{"mean"}{Mean value of attributes (multi-valued).}
#'    \item{"median"}{Median value of attributes (multi-valued).}
#'    \item{"min"}{Minimum value of attributes (multi-valued).}
#'    \item{"nrCorAttr"}{Number of attributes pairs with high correlation.}
#'    \item{"nrNorm"}{Number of attributes with normal distribution. The 
#'    Shapiro-Wilk Normality Test is used to assess if an attribute is or not is
#'    normally distributed.}
#'    \item{"nrOutliers"}{Number of attributes with outliers values. The 
#'    Turkey's boxplot algorithm is used to compute if an attributes has or does 
#'    not have outliers.}
#'    \item{"range"}{Range of Attributes (multi-valued).}
#'    \item{"sd"}{Standard deviation of the attributes (multi-valued).}
#'    \item{"skewness"}{Skewness of attributes (multi-valued).}
#'    \item{"tMean"}{Trimmed mean of attributes (multi-valued). It is the 
#'    arithmetic mean excluding the 20\% of the lowest and highest instances.}
#'    \item{"var"}{Attributes variance (multi-valued).}
#'  }
#'  This method uses simple binarization to transform the categorical attributes
#'  when \code{transform=TRUE}.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Ciro Castiello, Giovanna Castellano, and Anna M. Fanelli. Meta-data: 
#'  Characterization of input features for meta-learning. In 2nd International 
#'  Conference on Modeling Decisions for Artificial Intelligence (MDAI), 
#'  pages 457 - 468, 2005.
#'
#'  Shawkat Ali, and Kate A. Smith. On learning algorithm selection for 
#'  classification. Applied Soft Computing, volume 6, pages 119 - 138, 2006.
#'
#' @examples
#' ## Extract all meta-features
#' statistical(iris[,1:4])
#'
#' ## Extract some meta-features
#' statistical(iris[1:4], c("cor", "nrNorm"))
#'
#' ## Extract all meta-features without summarize the results
#' statistical(iris[,1:4], summary=c())
#' 
#' ## Use another summarization function
#' statistical(iris[,1:4], summary=c("min", "median", "max"))
#' 
#' ## Do not transform the data (using only categorical attributes)
#' statistical(iris[,1:4], transform=FALSE)
#' @export
statistical <- function(...) {
  UseMethod("statistical")
}

#' @rdname statistical
#' @export
statistical.default <- function(x, features="all",
                                   summary=c("mean", "sd"), 
                                   transform=TRUE, ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(features[1] == "all") {
    features <- ls.statistical()
  }
  features <- match.arg(features, ls.statistical(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
  if(length(summary) == 0) {
    summary <- "non.aggregated"
  }

  if(transform) {
    x <- binarize(x)
  }

  x.cov = stats::cov(x)

  extra <- list(
    x.cov = x.cov,
    eigenvalues = base::eigen(x.cov)
  )

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, list(x=x, extra=extra))
    post.processing(measure, summary, f %in% ls.statistical.multiples(), ...)
  }, simplify=FALSE)
}

#' List the statistical meta-features
#'
#' @return A list of statistical meta-features names
#' @export
#'
#' @examples
#' ls.statistical()
ls.statistical <- function() {
  c("cor", "cov", "eigenvalues", "gMean", "hMean", "iqRange", "kurtosis", "mad",
    "max", "mean", "median", "min", "nrCorAttr", "nrNorm", "nrOutliers", 
    "range", "sd", "skewness", "tMean", "var")
}

ls.statistical.multiples <- function() {
  c("cor", "cov", "eigenvalues", "gMean", "hMean", "iqRange", "kurtosis", "mad",
    "max", "mean", "median", "min", "range", "sd", "skewness", 
    "tMean", "var")
}

m.cor <- function(x, ...) {
  args <- list(...)
  method <- ifelse(is.null(args$method), "pearson", args$method)
  aux <- stats::cor(x, method=method)
  abs(aux[upper.tri(aux)])
}

m.cov <- function(x, extra, ...) {
  abs(extra$x.cov[upper.tri(extra$x.cov)])
}

m.eigenvalues <- function(x, extra, ...) {
  extra$eigenvalues$values
}

m.gMean <- function(x, ...) {
  res1 <- apply(x, 2, prod)^(1/nrow(x))

  x[x < 1] <- NA
  res2 <- apply(x, 2, function(col) {
    exp(base::mean(log(col), ...))
  })

  coalesce(res1, res2)
}

m.hMean <- function(x, ...) {
  apply(x, 2, function(col) length(col) / sum(1/col))
}

m.iqRange <- function(x, ...) {
  apply(x, 2, stats::IQR)
}

m.kurtosis <- function(x, ...) {
  apply(x, 2, e1071::kurtosis)
}

m.mad <- function(x, ...) {
  apply(x, 2, stats::mad)
}

m.max <- function(x, ...) {
  apply(x, 2, base::max)
}

m.mean <- function(x, ...) {
  apply(x, 2, base::mean)
}

m.median <- function(x, ...) {
  apply(x, 2, stats::median)
}

m.min  <- function(x, ...) {
  apply(x, 2, base::min)
}

m.nrCorAttr <- function(x, ...) {
  sum(abs(m.cor(x, ...)) >= 0.5) / (ncol(x) * (ncol(x) - 1) / 2)
}

m.nrNorm <- function(x, ...) {
  sum(unlist(apply(x, 2, function(col) {
    p.value <- NA
    tryCatch(
      p.value <- stats::shapiro.test(col[seq(min(length(col), 5000))])$p.value, 
      error = function(e) e
    )
    p.value
  })) > 0.1, na.rm = TRUE)
}

m.nrOutliers <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  sum(apply(x, 2, function(x) {
    qs <- stats::quantile(x, na.rm=na.rm)
    iqr <- (qs[4] - qs[2]) * 1.5
    (qs[2] - iqr) > qs[1] | (qs[4] + iqr) < qs[5] 
  }))
}

m.range <- function(x, ...) {
  res <- apply(x, 2, base::range)
  res[2,] - res[1,]
}

m.sd <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::sd, na.rm=na.rm)
}

m.skewness <- function(x, ...) {
  apply(x, 2, e1071::skewness)
}

m.tMean <- function(x, ...) {
  apply(x, 2, base::mean, trim=0.2)
}

m.var <- function(x, ...) {
  apply(x, 2, stats::var)
}
