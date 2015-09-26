average_vector <- function(x){

	# check input
	if (is.matrix(x) == FALSE){
		if (is.data.frame(x) == TRUE){
			x <- as.matrix(as.numeric(x))
		} else (stop(call. = FALSE,
			"x needs to be either a matrix or a numeric dataframe"))
	}
	# data will be in the form of rows = vectors, columns = components
	means <- as.vector(colMeans(x))
	return(means)
}