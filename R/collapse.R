#' Aggregate/collapse a grouped dataframe to an average
#'
#' description
#'
#' @param grouped_data grouped dataframe
#' @param average average function
#' @param metadata_prefix string, prefix of metadata columns
#' @param ... extra arguments to average function
#'
#' @import dplyr
#' @export
collapse <- function(grouped_data, average = median,
                      metadata_prefix = "Metadata_", ...) {

    if (!is_grouped_df(grouped_data)) {
        stop("`aggregate` expects a grouped dataframe.", call. = FALSE)
    }

    feature_cols = get_feature_cols(grouped_data, metadata_prefix)
    metadata_cols = get_metadata_cols(grouped_data, metadata_prefix)

    agg_feature_data = grouped_data %>%
        summarise_at(
            vars(feature_cols),
            funs(average(., ...))) %>%
        ungroup()

    agg_metadata = grouped_data %>%
        select(metadata_cols) %>%
        filter(row_number() == 1) %>%
        ungroup()

    merged_data = inner_join(agg_feature_data, agg_metadata,
                             by=group_vars(grouped_data))
    # make as many rows as original groups
    stopifnot(n_groups(grouped_data) == nrow(merged_data))
    # make sure the columns are in the original order
    merged_data[colnames(grouped_data)]
}


# TODO: check if all columns are homogenous within `data`
# i.e need the metadata *within* each group to be the same
# for the aggregation
is_homogenous <- function(data) {
    ans = all(apply(data, 2, function(x) {length(unique(x)) == 1}))
    if (ans != TRUE) {
        stop("Metadata columns are not homogenous within groups",
             call. = FALSE)
    }
}
