#' Calculate the norm (or length) of a vector
#' 
#' Calculates the length of a vector, also known as the norm if there are more
#' than two elements in the vector.
#' 
#' @param x Vector
#' 
#' @return Numeric, norm of the input vector
#' 
#' @export
#'
#' @examples
#' x <- c(3, 8)
#' norm_vector(x)
#' 
#' x <- c(3, 4, 5, 2)
#' norm_vector(x)

norm_vector <- function(x){

	if (is.numeric(x) == FALSE){
		stop(call. = FALSE,
			"Input needs to be a number.")
	}
	if (length(x) < 2){
		stop(call. = FALSE,
			"Needs to be a vector of a least 2 elements.")
	}

	# calculate the length (norm) of a vector
	norm_vec_out <- as.vector(sqrt(x %*% x))
	return(norm_vec_out)
}