## Installation

It is possible to install the development version using:

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("lpfgarcia/umfe")
library("umfe")
```

## Example of use

The simplest way to extract meta-features is using the `metafeatures` method. The method can be called by a symbolic description of the model or by a data frame. The parameters are the dataset and the group of measures to be extracted. The default parameter is extract all the measures. To extract a specific measure, use the function related with the group. A simple example is given next:

```r
## Extract all measures using formula
metafeatures(iris[,1:4])

## Extract general, statistical and information-theoretic measures
metafeatures(iris[,1:4], groups=c("general", "statistical", "infotheo"))

## Show the the available groups
ls.metafeatures()
```

Several measures return more than one value. To aggregate the returned values, post processed methods can be used. This method can compute min, max, mean, median, kurtosis, standard deviation, among others (see the `post.processing` documentation for more details). The default methods are the `mean` and the `sd`. Next, it is possible to see an example of the use of this method:

```r
## Extract all measures using min, median and max 
metafeatures(iris[,1:4], summary=c("min", "median", "max"))
                          
## Extract all measures using quantile
metafeatures(iris[,1:4], summary="quantile")
```
