read_map <- function (data, map,
                      verbose = TRUE,
                      new_col_name = "header"){
  require(dplyr)
  if (is.character(map) == TRUE) {
    map <- as.matrix(read.csv(map, header = FALSE))
  }
  map <- as.matrix(map)
  platemap <- mutate(data,
                     row = as.numeric(match(toupper(substr(data$well,1, 1)), LETTERS)),
                     column = as.numeric(substr(data$well, 2, 5)))
  
  platemap$header <- NULL
  
  for (i in 1:nrow(data)) {
    platemap$header[i] <- as.vector(with(platemap,
                                         map[[row[i],column[i]]]))
  }
  
  names(platemap)[ncol(platemap)] <- new_col_name
  
  if (verbose == FALSE) {
    platemap$row <- NULL
    platemap$column <- NULL
    return(platemap)
  } else if (verbose == TRUE) {
    return(platemap)
  } else stop("Argument 'verbose' requires a boolean.", call. = FALSE)
}