###############################################################################
# read_map will annotate dataframes with existing well ID
# with either concentration or compound from a layout-style csv
# for either a 96 or 384-well plate
#------------------------------------------------------------------------------
# will require an existing plate map in csv format; missing wells have to be 
# filled with placeholder to preserve row and column spacings
#------------------------------------------------------------------------------
# 'wells' is the column containing the well labels. format: 'A01'
# 'map' is the matrix of the plate map
###############################################################################

read_map <- function(wells, map){
    
    require(dplyr)
    
    # error handling for map if entered as file-path rather than matrix or df
    if(is.character(map) == TRUE){map <- as.matrix(read.csv(map, header = FALSE))}
    map <- as.matrix(map)
    
    # produces column and row numbers for given well ID
    platemap <- mutate(
        wells,
        row = as.numeric(match(toupper(substr(well,1,1)),LETTERS)),
        column = as.numeric(substr(well,2,5))
    )
    
    platemap$header <- NULL # dummy column for mapped values
    for(i in 1:nrow(wells)){
        platemap$header[i] <- as.vector(with(platemap, map[[row[i], column[i]]]))
    }
    return(platemap)
}

