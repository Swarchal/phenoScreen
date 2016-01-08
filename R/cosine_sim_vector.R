#' Cosine similarity between two vectors
#'
#' Given two vectors, this function will calculate a cosine similarity.
#'
#' @param a vector
#' @param b vector
#'
#' @return out cosine similarity
#'
#' @export
#'
#' @examples
#' set.seed(12321)
#' x <- rnorm(20)
#' y <- rnorm(20)
#' cosine_sim_vector(x, y)

cosine_sim_vector<- function(a, b){

	# check inputs
	if (!is.vector(a) || !is.vector(b)){
		stop("Inputs need to be vectors")
	}
	if (!is.numeric(a) || !is.numeric(b)){
		stop("Inputs need to be numeric")
	}
	if (length(a) != length(b)){
		stop("'a' and 'b' need to be the same length")
	}

    out <- sum(a * b) / sqrt(sum(a^2) * sum(b^2))
    return(out)
}