#' Calculates a Z-factor for two distributions
#' 
#' Calculates a Z-factor of Z-prime for two distributions, used to assess the
#' separation between positive and negative controls in high-throughput screens.
#' A value > 0.5 is indicative of a strong assay.
#' 
#' @param positive Vector
#' @param negative Vector
#' 
#' @return z-factor
#' 
#' @export
#'
#' @examples
#' x <- rnorm(100, 100)
#' y <- rnorm(100, 10)
#' z_factor(x, y)

z_factor <- function(positive, negative){
  
  # calculates the z-factor between selected upper and lower bound groups
  # a value between 0.5 and 1 is considered robust
  
  positive <- as.vector(positive)
  negative <- as.vector(negative)
  
  mu_p <- mean(positive, na.rm = TRUE)
  mu_n <- mean(negative, na.rm = TRUE)    

  sigma_p <- sd(positive)
  sigma_n <- sd(negative)
  
  z_prime <- 1 - ((3*(sigma_p + sigma_n)) / abs(mu_p - mu_n))
  return(z_prime)
}
