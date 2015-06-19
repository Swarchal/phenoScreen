###############################################################################
# read_map will annotate dataframes with existing well ID
# with either concentration or compound from a layout-style csv
# for either a 96 or 384-well plate
#------------------------------------------------------------------------------
# will require an existing plate map in csv format; missing wells have to be 
# filled with placeholder to preserve row and column spacings
#------------------------------------------------------------------------------
# argument 'data' has to be a dataframe, with the well identifier column
# named as 'well'
# 'map' is the matrix of the plate map
###############################################################################

read_map <- function(data, map,
    verbose = TRUE){
    
    require(dplyr)
    
    # error handling for map if entered as file-path rather than matrix or df
    if(is.character(map) == TRUE){map <- as.matrix(read.csv(map, header = FALSE))}
    map <- as.matrix(map)
    
    # produces column and row numbers for given well ID
    platemap <- mutate(
        data,
        row = as.numeric(match(toupper(substr(well,1,1)),LETTERS)),
        column = as.numeric(substr(well,2,5))
    )
    
    platemap$header <- NULL # dummy column for mapped values
    for(i in 1:nrow(data)){
        platemap$header[i] <- as.vector(with(platemap, map[[row[i], column[i]]]))
    }
    if (verbose == FALSE){
        platemap$row <- NULL
        platemap$column <- NULL
        return(platemap)
    } else  if (verbose == TRUE) {
        return(platemap)
    } else stop("Argument 'verbose' requires a boolean.",
        call. = FALSE)
}

