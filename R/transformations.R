#' Generalised log transformation
#'
#' defined as \code{log(y + sqrt(y^2 + lambda)}, where lambda = 0 is a normal
#' log transformation
#'
#' @param x vector or matrix
#' @param lambda transformation parameter
#'
#' @export

glog <- function(x, lambda = 0) {
    
    x <- as.matrix(x)
    log(0.5 * (x + sqrt(x^2 + lambda^2)))
}
