###############################################################################
# read_map will annotate dataframes with existing well ID
# with either concentration or compound from a layout-style csv
# for either a 96 or 384-well plate
#
# will require an existing plate map in csv format;
# missing wells have to be filled with placeholder to preserve row and column
# spacings
#
#------------------------------------------------------------------------------
# 'df' is the dataframe to which the annotations will be added
# 'wells' is the column containing the well labels. format: 'A01'
# 'map' is the csv file of the plate map
# 'header' is an optional argument naming the column values produced from map
###############################################################################

read_map <- function(df, wells, map, header = "new_column"){

	map <- as.matrix(read.csv(file, header = FALSE))

	#initialise new column
	df$header <- NA

	# need some sort of co-ordinate system to relate rows and columns
	# dont want to have to grep for every single combination of row and column
	# create column for 'row' and column for 'column' identified with grep
	# then paste them together
	# can create intermediate dataframe if necessary and use merge()

}
