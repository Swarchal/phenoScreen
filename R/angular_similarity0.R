#' Cosine dissimilarity / Angular similarity against the origin
#' 
#' The normalised angle between a vector and the origin, bounded between 0 and 1
#' 
#' @param a Vector
#' @return out Angle similarity, between 0 and 1
#' 
#' @export
#'
#' @examples
#' a <- c(-1, 0)
#' angular_similarity0(a)
#'
#' # Vectors of length > 2
#' x <- c(1, 4, 2, 2.5)
#' angular_similarity0(x)

angular_similarity0 <- function(a){
    
    # check inputs
    if (!is.vector(a)){
        stop("Inputs need to be vectors")
    }
    if (!is.numeric(a)){
        stop("Inputs need to be numeric")
    }

    # need to compare against the origin
    # create origin matching the same length as a
    vector_len <- length(a)
    origin <- c(1, rep(0, vector_len - 1))



    similarity <- as.vector(a %*% origin / sqrt(sum(a^2) * sum(origin^2)))
    
    out <- 1 - ((acos(similarity)/ pi))
    return(out)
    
}