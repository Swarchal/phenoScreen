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


#' principal component analysis
#'
#' replace feature columns with principal components
#'
#' @param data dataframe
#' @param metadata_prefix string, prefix of metadata columns
#' @param n_components integer, number of principal components to return
#'      if left blank then will return all principal components which is
#'      equal to the number of feature columns
#' @param ...
#'
#' @importFrom stats prcomp
#' @export
pca <- function(data, metadata_prefix = "Metadata_", n_components = NULL, ...) {


    feature_cols = get_feature_cols(data, metadata_prefix)
    metadata_cols = get_metadata_cols(data, metadata_prefix)
    metadata = data[, metadata_cols]
    
    # if n_components is not specified then return all principal components
    if (is.null(n_components)) {
        n_components = length(feature_cols)
    }
    
    if (n_components > length(feature_cols)) {
        stop("n_components > number of feature columns", call. = FALSE)
    }

    pc_comps = prcomp(data[, feature_cols], ...)$x[, 1:n_components]

    # create column names for the principal component dataframe
    colnames(pc_comps) = sprintf("PC%d", 1:n_components)

    # merge principal components and metadata 
    cbind(pc_comps, metadata)
}