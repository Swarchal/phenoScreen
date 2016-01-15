#' Convert cosine similarity to angular similarity
#'
#' If a cosine similarity has been calculated by cos_sim, then this function
#' will convert the value to angular similarity. Angular similarity is bound
#' between 0 and 1, with 1 being most similar, 0 being dissimilar, 0 being
#' orthogonal
#'
#' @param x vector of cosine similarity values
#' @return disim vector of angular similarity values
#'
#' @export
#'
#' @examples
#' vals <- runif(100, -1, 1) # random values
#' cossim_to_angsim(values)

cossim_to_angsim <- function(x){

	# check range of values, give warning if outside
	# possible cosine similarity values
	range_x <- range(x)
	if (range_x[1] < -1 || range_x[2] > 1){
		warning("The values are outside the expected range of -1 to 1")
	}

	disim <- 1 - acos(x) / pi
	return(disim)	
}


#' Convert angular similarity to cosine similarity
#'
#' If an angular similarity has been calculated, then this will convery it to
#' cosine similarities. A cosine similarity is bound between -1 and 1, with
#' 1 being similar and -1 being similar, 0 being orthogonal.
#'
#' @param x vector of angular similarities
#' @return similarity vector of cosine similarities
#'
#' @export
#'
#' @examples
#' vals <- runif(100) # 100 values between 0 and 1
#' angsim_to_cossim(vals)

angsim_to_cossim <- function(x){

	# check range of values, should be between 0 and 1
	# return warning if outside those values
	range_x <- range(x)
	if (range_x[1] < 0 || range_x[2] > 1){
		warning("The values are outside the expected range of 0 to 1")
	}

	similarity <- - cos(x * pi)
	return(similarity)
}