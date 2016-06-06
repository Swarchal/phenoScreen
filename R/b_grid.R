#' Plots multiple b-scored normalised platemaps 
#' 
#' Transforms numerical values using the b-score normalisation process to 
#' account for row and column effects. Uses well and plate labels to plot the
#' normalised values in the form of microtitre plates. Works for 96, 384 and
#' 1536 well plates.
#' 
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @return ggplot plot
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom Smisc list_to_dataframe
#' @export
#' 
#' @examples
#' df01 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 1)
#' 
#' df02 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 2)
#' 
#' df <- rbind(df01, df02)
#' 
#' b_grid(data = df$vals,
#'     well = df$well,
#'     plate_id = df$plate,
#'     plate = 96)

b_grid <- function(data, well, plate_id, plate = 96) {
    
    stopifnot(is.vector(data))
    
    # need to group_by plate_id, median polish, then return data.frame
    # that can be passed to ggplot and use raw_grid
    
    platemap <- plate_map_grid(data, well, plate_id)
    
    # force to factor
    platemap$plate_label <- as.factor(platemap$plate_label)
    
    # split by plate_id
    platemap_split <- split(platemap, platemap$plate_label)
    
    # apply med_smooth to each dataframe, split by plate_id
    med_smooth_list <- lapply(platemap_split, function(x){
        med_smooth(x, plate = plate)
    })
    
    print(med_smooth_list)
    
    # list to dataframe
    med_smooth_df <- list_to_dataframe(med_smooth_list,
                                       col_name = "plate_label")
   
    print(med_smooth_df)
    
    raw_grid(data = med_smooth_df$residual,
             well = med_smooth_df$well,
             plate_id = med_smooth_df$plate_label)
}