read_map <-
function(data, map){
    
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
    return(platemap)
}
