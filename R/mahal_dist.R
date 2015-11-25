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
