#' returns feature names to keep, without highly correlated features
#'
#' @param data data.frame
#' @param threshold correlation threshold
#' @importFrom stats cor
#'
#' @export
find_correlated <- function(data, threshold) {
    corr_data = cor(data)
    corr_data[upper.tri(corr_data)] = 0
    diag(corr_data) = 0
    colnames(data)[!apply(corr_data, 2, function(x) {any(abs(x) > threshold)})]
}

#' remove one of pairs of highly correlated features
#'
#' @param data data.frame
#' @param threshold correlation threshold
#' @param metadata_prefix string, prefix of metadata columns
#'
#' @import dplyr
#' @export
remove_correlated <- function(data, threshold = 0.95,
                              metadata_prefix = NULL) {

    metadata_prefix = get_metadata_prefix(metadata_prefix)
    featuredata = get_featuredata(data, metadata_prefix)
    metadata = get_metadata(data, metadata_prefix)
    cols_to_keep = find_correlated(featuredata, threshold)
    featuredata %>%
        select(cols_to_keep) %>%
        bind_cols(metadata)
}


# alias
rm_correlated = remove_correlated
