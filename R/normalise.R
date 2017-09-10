#' TODO: docstring
get_feature_index <- function(x, metadata_prefix = "Metadata") {
    setdiff(1:ncol(x), grep(metadata_prefix, colnames(x)))
}


get_feature_cols <- function(x, ...) {
    colnames(x)[get_feature_index(x, ...)]
}

#' TODO: docstring
set_operator <- function(method) {
    # set normalisation method, error if not valid
    if (method == "divide") {
        operator <- `/`
    } else if (method == "subtract") {
        operator <- `-`
    } else {
        stop("Invalid method. Options: divide, subtract.", call. = FALSE)
    }
    return(operator)
}


#' normalise against negative control
#'
#' description
#'
#' @param data dataframe, can be a grouped dataframe
#' @param compound_col name of column containing compound information
#' @param neg_compound name of the negative control compound in `compound_col`
#' @param method how to normalise, either "subtract" or "divide"
#' @param average average function
#' @param ... extras arguments passed to average
#'
#' @import dplyr
#' @export
normalise <- function(data, compound_col,
                      neg_compound = "DMSO", method = "subtract",
                      average = mean, metadata_prefix="Metadata_", ...) {

    `%op%` = set_operator(method)
    feature_cols = get_feature_cols(data, metadata_prefix)
    compound_col_ = enquo(compound_col)

    data %>%
        mutate_at(
            vars(feature_cols),
            funs(. %op% average(.[(!!!compound_col_) == neg_compound], ...)))
}
