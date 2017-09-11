#' return feature column indices
#' @param x data
#' @param metadata_prefix string, prefix of metadata columns
#' @export
get_feature_index <- function(x, metadata_prefix = "Metadata") {
    setdiff(1:ncol(x), get_metadata_index(x, metadata_prefix))
}


#' return feature column names
#' @param x data
#' @param ... arguments to `get_feature_index`
#' @export
get_feature_cols <- function(x, ...) {
    colnames(x)[get_feature_index(x, ...)]
}


#' return featuredata column indices
#' @param x data
#' @param metadata_prefix string, prefix of metadata columns
#' @export
get_metadata_index <- function(x, metadata_prefix = "Metadata") {
    grep(metadata_prefix, colnames(x))
}


#' return metadata column names
#' @param x data
#' @param ... arguments to `get_metadata_index`
#' @export
get_metadata_cols <- function(x, ...) {
    colnames(x)[get_metadata_index(x, ...)]
}