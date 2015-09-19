theta <- function(a, b){
    
    # computes the angle bewteen two vectors ('a' and 'b')
    norm_vec <- function(x) sqrt(x %*% x)
    
    theta <- as.vector(
        acos(a %*% b / (norm_vec(a) * norm_vec(b)))) * 180/pi
    return(theta)
}