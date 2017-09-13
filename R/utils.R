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


#' check featuredata is numeric
#'
#' @param data
#' @param feature_cols
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