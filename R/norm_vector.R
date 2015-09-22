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
	norm_vec_out <- sqrt(x %*% x)
	return(norm_vec_out)
}