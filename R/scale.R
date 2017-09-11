#' scale feature columns
#'
#' scale each feature column by z-score
#'
#' @param data
#' @param metadata_prefix
#' @param ... additional arguments to `z_score`
#' 
#' @export
scale_features <- function(data, metadata_prefix = "Metadata_", ...) {

    feature_cols = get_feature_cols(data, metadata_prefix)

    data %>%
        mutate_at(
            vars(feature_cols),
            funs(z_score(., ...))
        )
}


#' z_score
#'
#' standardise features to mean of 0 and unit variance
#'
#' @param x numeric vector
#' @param ... additional arguments to mean and sd, e.g na.rm = TRUE
#'
#' @importFrom stats mean
#' @importFrom stats sd
#' @export
z_score <- function(x, ...) {
    (x - mean(x, ...)) / sd(x, ...)
}