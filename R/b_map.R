#' Plots a heatmap of b-score normalised values in a plate layout
#'
#' Transforms numerical values using the b-score normalisation process to account
#' for row and column effects. Uses well labels to plot the normalised values in
#' the form of a microtitre plate. Works for either 96 or 384 well plates
#'
#' @param data Numerical values in the form of a vector to be normalised
#' @param well Vector of well identifiers, e.g "A01"
#' @param plate Plate format, either 96 or 384
#' @param normalise Not currently used
#' @param title Title of the plot
#' @param palette RColorBrewer palette
#' @return ggplot plot
#'
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @export
#'
#' @examples
#' df <- data.frame(well = num_to_well(1:96),
#' vals = rnorm(96))
#'
#' b_map(data = df$vals,
#'      well = df$well,
#'      plate = 96,
#'      title = "Plot Title")
#'
#' df_384 <- data.frame(
#'   well = num_to_well(1:384, plate = 384),
#'   vals = rnorm(384))
#'       
#' b_map(data = df_384$vals,
#'      well = df_384$well,
#'      plate = 384)


 b_map <- function(data, well,
                  normalise = FALSE,
                  plate = 96,
                  title = "",
                  palette = "Spectral"){
    
    
    # b_score() to obtain residual values
    #--------------------------------------------------------------------------

    stopifnot(is.vector(data))
    
    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    
    platemap <- plate_map(data, well)
    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$Row, platemap$Column), ]
    
    
    if (length(well) > plate){
        warning("Invalid plate selection. The data given has more rows then number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate > 2 * length(well)){
        warning("Plate has greater than twice the number of wells than data points. \nAre you sure this is the correct plate? \nDefault argument is 96.",
                call. = FALSE)
    }

    df <- med_smooth(platemap, plate)
    
    df$values <- scale(df$residual)
    platemap <- plate_map(df$values, df$well)
    
    # produce a plate map in ggplot (96-well format)
    if (plate == 96){
        plt <- plt96(platemap) +
            scale_fill_distiller("z-score", palette = palette) +
            ggtitle(title)+
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