#' impute missing values
#'
#' @param data dataframe
#' @param metadata_prefix string, prefix for metadata columns
#' @param method string, method of imputation. Either "mean", "median", or "nn"
#'      (nearest-neighbour).
#'
#' @export
# FIXME: doesn't work with grouped dataframes
impute_missing <- function(data, metadata_prefix = NULL, method="mean") {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    check_impute_method(method)
    # run appropriate function given for a given method
    switch(
        method,
        mean   = impute_mean(data, metadata_prefix),
        median = impute_median(data, metadata_prefix),
        nn     = impute_nn(data, metadata_prefix)
    )
}


# check method argument is valid
check_impute_method <- function(method) {
    valid_methods = c("mean", "median", "nn")
    if (method %in% valid_methods == FALSE) {
        stop(sprintf("%s is not in valid methods: (%s)",
                     method, paste(valid_methods, collapse = ", ")),
             call.=FALSE)
    }
}


# impute missing values with the column mean
impute_mean <- function(data, metadata_prefix) {
    feature_cols = get_feature_cols(data, metadata_prefix)
    data %>%
        mutate_at(feature_cols, funs(
            ifelse(is.na(.), mean(., na.rm = TRUE), .)
            )
        )
}


# impute missing values with the column median
impute_median <- function(data, metadata_prefix) {
    feature_cols = get_feature_cols(data, metadata_prefix)
    data %>%
        mutate_at(feature_cols, funs(
            ifelse(is.na(.), median(., na.rm = TRUE), .)
            )
        )
}


# impute missing values with the value of the nearest-neighbour that has a
# value for that column
impute_nn <- function(data, metadata_prefix) {
    # TODO: make this
    stop("not made this yet!")
}


#' drop rows with missing values
#'
#' @param data dataframe
#' @param threshold float remove row(s) if the proportion of missing values
#'      equals or exceeds `threshold`
#' @param metadata_prefix string, prefix for metadata columns
#'
#' @export
drop_missing_rows <- function(data, threshold, metadata_prefix = NULL) {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    feature_data = get_featuredata(data, metadata_prefix)
    na_count = apply(feature_data, 1, function(x){sum(is.na(x))})
    rows_to_rm = which(na_count / ncol(feature_data) >= threshold)
    return(data[-rows_to_rm, ])
}


#' drop feature columns with missing values
#'
#' @param data dataframe
#' @param threshold float remove column(s) if the proportion of missing values
#'      equals or exceeds `threshold`
#' @param metadata_prefix string, prefix for metadata columns
#'
#' @export
drop_missing_cols <- function(data, threshold, metadata_prefix = NULL) {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    feature_data = get_featuredata(data, metadata_prefix)
    na_count = apply(feature_data, 2, function(x){sum(is.na(x))})
    # cant remove columns by index, as using feature data subset and the
    # original indices won't match up -- have to use names instead
    cols_to_rm_idxs = which(na_count / nrow(feature_data) >= threshold)
    cols_to_rm = colnames(feature_data)[unique(cols_to_rm_idxs)]
    return(data[, setdiff(colnames(data), cols_to_rm)])
}
