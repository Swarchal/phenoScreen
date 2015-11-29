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
    
    require(ggplot2)
    require(dplyr)
    require(RColorBrewer)

    if (!is.vector(data)){
        stop("'data' has to be a single column or a vector")
    }
    
    # transform well labels into row-column values for a 96-well plate
    platemap <- as.data.frame(well)
    names(platemap)[1] <- "well"
    platemap <- mutate(platemap,
                       Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                       Column = as.numeric(substr(well, 2, 5)))
    
    values <- as.data.frame(data)
    scaled_data <- scale(values)
    platemap <- cbind(platemap, scaled_data[,1])
    names(platemap)[4] <- "scaled_data"
    
    if (plate == 96){
        # produce a plate map in ggplot (96-well format)
        plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
            geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                       color = "grey90", fill = "white", shape = 21, size = 6) +
            geom_point(aes(fill = scaled_data), colour = "gray20", shape = 21, size = 10) +
            coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
            scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
            scale_x_continuous(breaks = seq(1, 12)) +
            scale_fill_distiller("z-score", palette = palette) +
            ggtitle(title) +
            theme_bw()
            
    } else if (plate == 384){
        # produce a plate map in ggplot (384-well format)
        plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
            geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                       color = "grey90", fill = "white", shape = 22, size = 3) +
            geom_point(aes(fill = scaled_data), colour = "gray20", shape = 22, size = 5) +
            coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
            scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
            scale_x_continuous(breaks = seq(1, 24)) +
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
