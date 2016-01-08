#' Creates a cosine similarity matrix
#'
#' For a given matrix, will create a cosine similarity matrix
#'
#' @param X matrix
#'
#' @return C cosine similarity matrix
#'
#' @export
#'
#' @examples
#' # example data
#' mat <- iris[,1:4]
#' out <- cosine_sim_mat(mat)
#' image(out)


cosine_sim_mat <- function(X){

	cos_sim <- function(ix){
    A = X[ix[1],]
    B = X[ix[2],]
    return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
	}

	# if given a dataframe, will try and convert to a matrix
	if (is.data.frame(X)){
		X <- as.matrix(X)
		warning("Attempting to convert dataframe to a matrix")
	}

	# check input
	if (!is.matrix(X)) stop("X needs to be a matrix")
	if (!is.numeric(X)) stop("X needs to be numeric")


    n <- nrow(X) 
    cmb <- expand.grid(i = 1:n, j = 1:n) 
    C <- matrix(apply(cmb, 1, cos_sim), n, n)
    return(C)
}
