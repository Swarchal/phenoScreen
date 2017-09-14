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
#' @param n_components numeric, If an integer greater than or equal to 1, then 
#'      this many principal components will be returned.
#'      If a floating point number between 0 and 1, then the number of principal
#'      components will be how many needed to account for that proportion of
#'      variance in the data.
#'      If left blank then will return all principal components which is
#'      equal to the number of feature columns.
#' @param ... additional arguments to \code{prcomp}
#'
#' @importFrom stats prcomp
#' @export
pca <- function(data, metadata_prefix = NULL, n_components = NULL, ...) {

    metadata_prefix = get_metadata_prefix(metadata_prefix)
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

    # if n_components is an integer >=1 then select that many principal components
    if (is_integer(n_components) && n_components >= 1) {
        pc_comps = prcomp(data[, feature_cols], ...)$x[, 1:n_components]
        pc_comps = as_tibble(pc_comps)

    # if n_components is a float < 1, then get as many principal components
    # as needed to account for that proportion of variance in the data
    } else if (n_components < 1 && n_components > 0) {
        threshold = n_components
        pca_out = prcomp(data[, feature_cols, ...])
        pc_variance = pca_out$sdev^2
        cumulative_proportion_variance = cumsum(pc_variance) / sum(pc_variance)
        n_components = min(which(cumulative_proportion_variance >= threshold))
        pc_comps = as_tibble(pca_out$x[, 1:n_components])

    } else {
        stop("n_components need to be an integer >= 1, or a float between 0 and 1")
    }

    # create column names for the principal component dataframe
    colnames(pc_comps) = sprintf("PC%d", 1:n_components)

    # merge principal components and metadata 
    cbind(pc_comps, metadata)
}
