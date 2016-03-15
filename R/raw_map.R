#' Plots a platemap with heatmap of raw values
#' 
#' Converts numerical values and  well labels into multiple plate heatmaps
#' 
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96 or 384)
#' @param title Title of plot
#' @param palette RColorBrewer palette
#' 
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#'
#' @return ggplot plot
#'
#' @export
#' 
#' @examples
#' df <- data.frame(vals = rnorm(1:384),
#'   well = num_to_well(1:384, plate = 384))
#' 
#' raw_map(data = df$vals,
#'         well = df$well,
#'         title = "Title of plot")

raw_map <- function(data, well,
                    plate = 96,
                    title = "",
                    palette = "YlGnBu"){
  
    if (!is.vector(data)){
	stop("'data' has to be a single column or a vector")
    }
  
    # transform well labels into row-column values
    platemap <- plate_map(data, well)
  
    if (plate == 96){
	plt <- plt96(platemap) +
	scale_fill_distiller("values", palette = palette) +
	ggtitle(title) +
	theme_bw()
    } else if (plate == 384){
	plt <- plt384(platemap) +
	scale_fill_distiller("values", palette = palette) +
	ggtitle(title) +
	theme_bw()
    } else stop("Invalid argument for 'plate'. \nOption: 96 or 384",
	call. = FALSE)

    return(plt)
  
}
