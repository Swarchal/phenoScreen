#' Get featuredata columns
#'
#' Identifies columns in a dataframe that are not labelled Metadata
#'
#' @param x dataframe
#' @param metadata_prefix pattern to match metadata columns
#' @export

get_featuredata <- function(x, metadata_prefix = "Metadata"){
    setdiff(1:ncol(x), grep(metadata_prefix, colnames(x)))
}


#' Normalise per plate against negative control
#'
#' This function will normalise multivariate data on a plate-by-plate basis
#' by dividing each feature by the median of the negative control for
#' that feature. Note: Assumes any metadata columns are prefixed with
#' "Metadata_", and everything else is numerical featuredata
#'
#' @param df dataframe
#' @param plate_id string, name of the column of plate names, name for each plate
#' @param compound string, name of column of compound names
#' @param neg_compound, name of the negative control to normalise against
#'
#' @import dplyr
#' @export

normalise <- function(df, plate_id, compound = "Metadata_compound", neg_compound = "DMSO"){
    stopifnot(is.data.frame(df))
    
    # identify feature data columns
    feature_data <- get_featuredata(df)

    out <- df %>%
	group_by_(plate_id) %>%
	mutate_each_(funs(./median(.[compound == neg_compound], na.rm = TRUE)), feature_data)
	return(out)
}

#' Scale feature data
#'
#' Z-score of feature data, each features scaled separately
#'
#' @param df dataframe
#' @export

scale_features <- function(df){
    feature_data <- get_featuredata(df)
    apply(df[, feature_data], 2, scale)
}

