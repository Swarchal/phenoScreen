#' Aggregate a grouped dataframe to an average
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
aggregate <- function(grouped_data, average=median,
                      metadata_prefix="Metadata_", ...) {

    if (!is_grouped_data(grouped_data)) {
        stop("ERROR: `aggregate` expects a grouped dataframe.")
    }

    feature_cols = get_feature_cols(data, metadata_prefix)
    metadata_cols = get_metadata_cols(data, metadata_prefix)

    # check if groups have homogenous metadata
    grouped_data %>% select(metadata_cols) %>% is_homogenous()

    agg_feature_data = grouped_data %>%
        summarise_at(
            vars(feature_cols),
            funs(average(., ...))) %>%
        ungroup()

    agg_metadata = grouped_data %>%
        select(metadata_cols) %>%
        filter(row_number() == 1) %>%
        ungroup()

    merged_data = inner_join(agg_feature_data, agg_metadata)
    # make as many rows as original groups
    assert(n_groups(grouped_data) == nrow(merged_data))
    # make sure the columns are in the original order
    merged_data[colnames(grouped_data)]
}


# check if all columns are homogenous within `data`
# i.e need the metadata *within* each group to be the same
# for the aggregation
is_homogenous <- function(data) {
    ans = all(apply(data, 2, function(x) {length(unique(x)) == 1}))
    if (ans != TRUE) {
        stop("Metadata columns are not homogenous within groups", call. = FALSE)
    }
}
