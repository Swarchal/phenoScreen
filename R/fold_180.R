fold_180 <- function(x){
  if (x > 180){ 
    x <- x - 2 * (x - 180) # reduce 180 by the amount theta is greater than
  }
  return(x)
}