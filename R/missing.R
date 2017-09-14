#' impute missing values
#'
#' @param data dataframe
#' @param metadata_prefix string, prefix for metadata columns
#'
#' @export
impute_missing <- function(data, metadata_prefix = NULL) {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    stop("not made this yet")
}


#' drop rows/columns with missing values
#'
#' @param data dataframe
#' @param metadata_prefix string, prefix for metadata columns
#'
#' @export
drop_missing <- function(data, metadata_prefix = NULL) {
    metadata_prefix = get_metadata_prefix(metadata_prefix)
    stop("not made this yet!")
}
