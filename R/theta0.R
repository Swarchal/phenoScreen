#' Angle between a vector and the origin
#' 
#' Calculates the angle (in degrees) between a vector and the origin (1,0)
#' 
#' @param a Vector
#' @return Angle, in degrees
#' 
#' @examples
#' a <- c(1, 0)
#' theta0(a)
#' 
#' a <- c(-1, 0)
#' theta0(a)
#' 
#' a <- c(-1, -1)
#' theta0(a)

theta0 <- function(a){
    # computes the angle between a vector and the origin
    
    # length/norm of a vector 'x'
    norm_vec <- function(x) sqrt(x %*% x)
    
    # origin vector
    origin <- c(1, 0)
    
    theta <- as.vector(
        acos(a %*% origin / (norm_vec(a) * norm_vec(origin))) * 180/pi
        )
    
    # need to measure the angle from a set reference, so it's always anticlockwise
    # otherwise theta never exceed 180
    
    if (a[2] >= 0){
        return(theta)
    }
    
    if (a[2] <0){
        return(360 - theta)
    }

}