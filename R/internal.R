binarize <- function(x) {
  att <- paste(colnames(x), collapse=" + ")
  x <- stats::model.matrix(stats::formula(paste("~ 0 +", att, sep=" ")), x)
  data.frame(x)
}

categorize <- function(x) {
  att <- sapply(x, is.numeric)
  x <- cbind(x[!att], infotheo::discretize(x[att]))
  data.frame(sapply(x, as.factor))
}

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x
  }, list(...))
}

concentration.coefficient <- function(x, y) {
  nij <- table(y, x) / length(x)
  isum <- rowSums(nij)
  jsum2 <- sum(colSums(nij)^2)
  nij2 <- nij^2
  
  (sum(nij2 / isum) - jsum2) / (1 - jsum2)
}

entropy <- function(x) {
  qi <- table(x) / length(x)
  -sum(qi * sapply(qi, log2))
}
