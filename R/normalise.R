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
#' \code{metadata_prefix} , and everything else is numerical featuredata
#'
#' @param df dataframe
#' @param plate_id string, name of the column of plate names, name for each plate
#' @param compound string, name of column of compound names
#' @param neg_compound, string, name of the negative control to normalise against
#' @param method string, How to normalise the data. Options are divide or
#'	subtract. e.g subtract DMSO median from values, or divide by DMSO values.
#' @import dplyr
#' @importFrom lazyeval interp
#' @export
#' @examples
#' # example data
#' N_PLATES <- 5
#' wells <- rep(num_to_well(1:96), N_PLATES)
#' plate_id <- rep(c("plate_1", "plate_2", "plate_3", "plate_4", "plate_5"),
#' 		each = 96)
#' val1 <- rnorm(96 * N_PLATES, 10, 10)
#' val2 <- rnorm(96 * N_PLATES, 1, 100)
#' comps <- c(rep("cmpd", 80), rep("DMSO", 16))
#' compound <- rep(comps, N_PLATES)
#' 
#' df <- data.frame(Metadata_well = wells,
#' 		 Metadata_plate_id = plate_id,
#' 		 Metadata_compound = compound,
#' 		 val1, val2)
#' 
#' df_out <- normalise(df,
#' 		    plate_id = "Metadata_plate_id",
#' 		    compound = "Metadata_compound",
#' 		    neg_compound = "DMSO")
#' 


normalise <- function(df, plate_id, compound = "Metadata_compound", neg_compound = "DMSO",
		      method = "divide"){
    stopifnot(is.data.frame(df))
    
    if (method == "divide"){
	`%op%` <- `/`
    } else if (method == "subtract"){
	`%op%` <- `-`
    } else{
	stop("Not a valid method. Options: divide or subtract",
	     call. = FALSE)
    }

    # identify feature data columns
    feature_data <- get_featuredata(df)

    out <- df %>%
	group_by_(plate_id) %>%
	mutate_each(funs_(interp(~. %op% median(.[x == neg_compound], na.rm = TRUE),
			  x = as.name(compound))), feature_data) %>%
	ungroup()
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

