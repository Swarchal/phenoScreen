# get metadata_prefix from options if is null
get_metadata_prefix <- function(metadata_prefix) {
    if (is.null(metadata_prefix)) {
        metadata_prefix <- getOption("metadata_prefix")
    }
    metadata_prefix
}


#' return feature column indices
#' @param x data
#' @param metadata_prefix string, prefix of metadata columns
#' @export
get_feature_index <- function(x, metadata_prefix = NULL) {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    setdiff(1:ncol(x), get_metadata_index(x, metadata_prefix))
}


#' return feature column names
#' @param x data
#' @param ... arguments to `get_feature_index`
#' @export
get_feature_cols <- function(x, ...) {
    colnames(x)[get_feature_index(x, ...)]
}


#' return featuredata
#' @param x data
#' @param ... arguments to \code{get_feature_cols}
#' @export
get_featuredata <- function(x, ...) {
    x[, get_feature_cols(x, ...)]
}


#' return featuredata column indices
#' @param x data
#' @param metadata_prefix string, prefix of metadata columns
#' @export
get_metadata_index <- function(x, metadata_prefix = NULL) {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    grep(metadata_prefix, colnames(x))
}


#' return metadata column names
#' @param x data
#' @param ... arguments to `get_metadata_index`
#' @export
get_metadata_cols <- function(x, ...) {
    colnames(x)[get_metadata_index(x, ...)]
}


#' return metadata
#' @param x data
#' @param ... arguments to \code{get_metadata_cols}
#' @export
get_metadata <- function(x, ...) {
    x[, get_metadata_cols(x, ...)]
}


#' check featuredata is numeric
#'
#' @param data dataframe
#' @param feature_cols vector of string, feature column names
#' @export
check_feature_data <- function(data, feature_cols) {
    featuredata = data[, feature_cols]
    is_numeric = unlist(Map(is.numeric, featuredata))
    if (sum(is_numeric) < ncol(featuredata)) {
        # find non-numeric column(s)
        non_numeric_col = names(is_numeric[is_numeric == FALSE])
        stop(sprintf("non numeric feature data columns found %s",
                     non_numeric_col))

    }
}


# because R is stupid and is.integer doesn't do what you think it does
is_integer <- function(x) {
    as.integer(x) == x
}
