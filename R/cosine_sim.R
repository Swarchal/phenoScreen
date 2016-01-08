#' Cosine similarity 
#' 
#' Calculates a cosine similarity of either a matrix or between two vectors.
#' If given a matrix, will return a cosine-similarity matrix. If given two
#' vectors will calculate the cosine-similarity between the two. Specific
#' functions for cosine vectors or matrices can be called with
#' \code{cosine_sim_vector} and \code{cosine_sim_mat} respectively.
#'
#' @param ... Either a numerical matrix, or two numerical vectors of the same
#' 		length.
#' 
#' @return out A matrix if given a matrix, or a single number if given two
#' 		vectors.
#'
#' @export
#'
#' @examples
#' # example matrix
#' mat <- as.matrix(iris[,1:4])
#' out <- cosine_sim(mat)
#' image(out)
#'
#' # two vectors
#' a <- rnorm(20)
#' b <- rnorm(20)
#' cosine_sim(a, b)

cosine_sim <- function(...){

    dots <- list(...)
    if (length(dots) == 1){
        # a matrix
        out <- cosine_sim_mat(dots[[1]])
    } else if (length(dots) == 2){
    	# two vectors
        out <- cosine_sim_vector(dots[[1]], dots[[2]])
    } else stop("Need either a matrix or two vectors")

    return(out)
}
