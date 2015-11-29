#' Angle between two vectors
#' 
#' Calculates the angle between two vectors, output in degrees
#' 
#' @param a Vector
#' @param b Vector
#' 
#' @return Angle in degrees
#' 
#' @examples
#' a <- c(1, 2)
#' b <- c(3, 1)
#' 
#' theta(a,b)
#' 
#' a <- c(1, 2)
#' b <- c(-1, -2)
#' theta(a, b)

theta <- function(a, b){
    
    # computes the angle bewteen two vectors ('a' and 'b')
    norm_vec <- function(x) sqrt(x %*% x)
    
    theta <- as.vector(
        acos(a %*% b / (norm_vec(a) * norm_vec(b)))) * 180/pi
    return(theta)
}