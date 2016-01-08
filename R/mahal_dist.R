#' Mahalanobis Distance
#' 
#' Calculates a Mahalanobis distance of a dataframe or a matrix.
#' Option for robust centroid location.The Mahalanobis distance is a
#' measure of distance between a point and a distribution. In this
#' simplified case it's the difference between each point and centroid
#' of the distribution produced by the points.
#' 
#' @param x Dataframe or matrix of numerical values
#' @param center Method to calculate distribution center, either mean or median
#' @param na.rm Whether to remove missing values for centroid calculation
#' 
#' @return vector of distances
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' 
#' iris_values <- iris[,1:4]
#' mahal_dist(iris_values)
#' mahal_dist(iris_values, center = "median")

mahal_dist <- function (x, center = "mean", na.rm = TRUE){
  
  x <- if (is.vector(x)){
    matrix(x, ncol = length(x))
  }else as.matrix(x)
  
  # function for colMedian
  colMedian <- function(x, ...) apply(x, 2, median, na.rm = na.rm)
  
  if (center == "mean"){
    center <- colMeans(x, na.rm = na.rm)
    
  } else if (center == "median"){
    center <- colMedian(x, na.rm = na.rm)
    
  }else stop("Not a valid center argument\nOptions: 'mean' or 'median'")
  
  cov <- cov(x)
  
  if (!identical(center, FALSE)){
    x <- sweep(x, 2L, center)}
  
  cov <- solve(cov)
  
  setNames(rowSums(x %*% cov * x), rownames(x))
}
