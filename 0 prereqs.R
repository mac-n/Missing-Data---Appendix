#test imputation methods

imputecolmed<-function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
require(mice)
require(missForest)
require(pcaMethods)
require(beepr)
require(caret)