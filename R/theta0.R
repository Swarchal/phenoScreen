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
    # so in vector(x,y), if y is negative; need to add 180 to theta
    # otherwise theta never exceed 180
    
    if (a[2] >= 0){
        return(theta)
    }
    
    if (a[2] < 0){
        return(theta + 180)
    }
}