#' Plots a platemap with heatmap of scaled values
#' 
#' Converts numerical values and  well labels into multiple plate heatmaps
#' 
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96 or 384)
#' @param title Title of plot
#' @param palette RColorBrewer palette
#' 
#' @return ggplot plot
#'
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @export
#'
#' @examples
#' df <- data.frame(vals = rnorm(1:384),
#'   well = num_to_well(1:384, plate = 384))
#' 
#' z_map(data = df$vals,
#'       well = df$well,
#'       plate = 384,
#'       title = "Title of plot")


z_map <- function(data, well,
    plate = 96,
    title = "",
    palette = "Spectral"){

    stopifnot(is.vector(data))
    
    platemap <- plate_map_scale(data, well)
    
    if (plate == 96){
        # produce a plate map in ggplot (96-well format)
        plt <- plt96(platemap) +
            scale_fill_distiller("z-score", palette = palette) +
            ggtitle(title) +
            theme_bw()
            
    } else if (plate == 384){
        # produce a plate map in ggplot (384-well format)
        plt <- plt384(platemap) +
            scale_fill_distiller("z-score", palette = palette) +
            ggtitle(title) +
            theme_bw()

    } else stop("Not a valid plate format. Enter either 96 or 384.", call. = FALSE)

    if (length(well) > plate) {
        stop("Invalid plate selection. The data given has more rows than number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is set to a 96-well plate.",
            call. = FALSE)
    } 

    return(plt)
}
