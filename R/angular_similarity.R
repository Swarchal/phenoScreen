#' Cosine dissimilarity / Angular similarity 
#' 
#' The normalised angle between two vectors, bounded between 0 and 1
#' 
#' @param a Vector
#' @param b Vector
#' @return out Angle similarity, between 0 and 1
#' 
#' @export
#'
#' @examples
#' a <- c(-1, 0)
#' b <- c(-1, -1)
#' angular_similarity(a, b)
#'
#' # Vectors of length > 2
#' x <- c(1, 4, 2, 2.5)
#' y <- c(5, 200, 4, -1)
#' angular_similarity(x, y)

angular_similarity <- function(a, b){
    
    # check inputs
    if (!is.vector(a) || !is.vector(b)){
        stop("Inputs need to be vectors")
    }
    if (!is.numeric(a) || !is.numeric(b)){
        stop("Inputs need to be numeric")
    }
    if (length(a) != length(b)){
        stop(paste(
            substitute(a), "and", substitute(b), "need to be the same length")
        )
    }
    
    similarity <- as.vector(a %*% b / sqrt(sum(a^2) * sum(b^2)))
    
    out <- 1 - ((acos(similarity)/ pi))
    return(out)
    
}