#' Platemap to identify 'hits' in a screen
#' 
#' Produces a plot in the form of a micro-titre layout,
#' with colours indicating wells above or below a nominated threshold.
#' 
#' @param data Vector of numerical values to score
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96 or 384)
#' @param threshold Numerical value of standard deviations from the mean
#'   for a well to be classified as a 'hit'. Default it +/- 2 SD
#' @param title Title of the plot
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
#' hit_map(data = df$vals,
#'        well = df$well,
#'        plate = 384,
#'        threshold = 3,
#'        title = "Awesome plot")



hit_map <- function(data, well,
     plate = 96,
     threshold = 2,
     title = "",
     palette = "Spectral"){
    
    stopifnot(is.vector(data))

    # transform well labels into row-column values for a 96-well plate
    platemap <- plate_map_scale(data, well)
    platemap$hit <- NA
    
    # calculate whether values are beyond the threshold; defined as hit or null
    for (row in 1:nrow(platemap)){
        if (platemap[row, 'values'] > threshold) {platemap$hit[row] <- "hit"
        } else  if (platemap[row, 'values'] < (-1 * threshold)){platemap$hit[row] <- "neg_hit"
        } else {platemap$hit[row] <- "null"}
    }
    
    # RColorBrewerPalette
    my_cols <- brewer.pal(3, palette)
    my_colours <- c(hit = my_cols[1], neg_hit = my_cols[3], null = my_cols[2])

    # change name of hit to values
    # plt96 and plt384 are colouring points by value, in this case needs to be hit
    platemap$actual_vales <- platemap$values
    platemap$values <- platemap$hit
    
    if (plate == 96){
        # produce a 96-well plate map layout in ggplot
        plt <- plt96(platemap) +
            ggtitle(title) + 
            scale_fill_manual("hit", values = my_colours) + 
            theme_bw()
    } else if (plate == 384){
        # produce a 384-well plate map layout in ggplot
        plt <- plt384(platemap) +
            ggtitle(title) +
            scale_fill_manual("hit", values = my_colours) + 
            theme_bw()
    } else stop("Not a valid plate format. Either 96 or 384.", call. = FALSE)

    if (length(well) > plate) {
        stop("Invalid plate selection. The data given has more rows than the number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is set to a 96-well plate.")
    }

    return(plt)

}
