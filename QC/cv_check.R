#################################################################
# cv_check
#----------------------------------------------------------------
# calculates the coefficient of variation for a given data set
# will require a given column and factor to subset data by
# e.g date or plate no.
# producing a data frame of CV for every element of the group
#
# example: cv_check(df$count, df$date)
################################################################

cv_check <- function(data, group){

	require(raster)

	cv_df <- aggregate(data, list(group = group), cv)
	names(cv_df)[2] <- "CV"
	return(cv_df)
}

