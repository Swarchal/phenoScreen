#' normalise against negative control
#'
#' description
#'
#' @param data dataframe, can be a grouped dataframe
#' @param compound_col name of column containing compound information
#' @param neg_control name of the negative control compound in `compound_col`
#' @param method how to normalise, either "subtract" or "divide"
#' @param average average function
#' @param metadata_prefix string, prefix of metadata columns
#' @param ... extras arguments passed to average
#'
#' @import dplyr
#' @importFrom stats median
#' @export
normalise <- function(data, compound_col,
                      neg_control = "DMSO", method = "subtract",
                      average = median, metadata_prefix = NULL, ...) {

    metadata_prefix = get_metadata_prefix(metadata_prefix)
    `%op%` = set_operator(method)
    feature_cols = get_feature_cols(data, metadata_prefix)
    compound_col_ = enquo(compound_col)

    data %>%
        mutate_at(
            vars(feature_cols),
            funs(. %op% average(.[(!!!compound_col_) == neg_control], ...)))
}


# alias for American spelling
normalize = normalise


# internal function to set operator
set_operator <- function(method) {
    # set normalisation method, error if not valid
    if (method == "divide") {
        operator = `/`
    } else if (method == "subtract") {
        operator = `-`
    } else {
        stop("Invalid method. Options: divide, subtract.",
             call. = FALSE)
    }
    return(operator)
}
