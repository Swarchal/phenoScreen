#' Converts numbers to well labels
#' 
#' Converts numerical values to corresponding alpha-numeric well labels
#' for either 96 or 384 well plates.
#' 
#' @param x Vector of numbers to be converted
#' @param plate Number of wells in complete plate (96 or 384)
#' 
#' @return Vector of alpha-numeric well labels
#'
#' @export
#'  
#' @examples
#' num_to_well(1:96)
#' num_to_well(1:96, plate = 384)
#' 
#' nums <- c(1:10, 20:40, 60:96)
#' num_to_well(nums)

num_to_well <- function(x, plate = 96){
  
  if (plate == 96L){
    rows <- LETTERS[1:8]
    columns <- 1:12
  } else if (plate == 384L){
    rows <- LETTERS[1:16]
    columns <- 1:24 
  } else stop("Plate needs to be 96 or 384")
  
  # columns then rows for normal row-wise counting  
  combinations <-  expand.grid(columns, rows)
  # but then have to reverse order to print in the normal way
  out <- paste0(combinations[x, 2], combinations[x, 1]) 
  return(out)
}