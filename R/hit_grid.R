#' Plots multiple platemaps with and identifies hits
#' 
#' Converts numerical values and well labels into 'hits' in the form of
#' multiple plate maps. Hits are calculated as wells above or below a 
#' specified number of standard deviations from the overall average
#' 
#' @param data Numerical values to be scaled and plotted
#' @param well Vector of well identifiers. e.g "A01"
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @param threshold Numerical value of standard deviations from the mean
#'  for a well to be classified as a 'hit'. Default it +/- 2 SD
#' @param ncols Number of columns in the grid of plates
#' @param plate Number of wells in the complete plates (96 or 384)
#' @param title Title of the plot
#' @param scale Not currently used
#' @param palette RColorBrewer palette
#' 
#' @return ggplot plot
#'
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
#' hit_grid(data = df$vals,
#'     well = df$well,
#'     plate_id = df$plate,
#'     plate = 96,
#'     title = "Plot Title")


hit_grid <- function(data, well,
                   plate_id,
                   threshold = 2,
                   ncols = 2,
                   plate = 96,
                   title = "",
                   scale = "all",
                   palette = "Spectral"){
    
    ## multiple platemap plots in a single figure using facet_wrap
    
    require(dplyr)
    require(ggplot2)
    require(RColorBrewer)
    require(grid)
    
    ## transform well labels into row-column values for a 96-well plate
    ## need to include plate_id labels into this dataframe
    ## scale for all values, or scale per plate??
    
    
    if (scale == "all"){
        
        # normalised across entire range of values
        platemap <- as.data.frame(well)
        names(platemap)[1] <- "well"
        platemap <- mutate(platemap,
                           Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                           Column = as.numeric(substr(well, 2, 5)))
        values <- as.data.frame(data)
        plate_label <- as.data.frame(plate_id)
        scaled_data <- scale(values)
        platemap <- cbind(platemap, scaled_data[,1], plate_id)
        names(platemap)[4] <- "scaled_data"
        names(platemap)[5] <- "plate_label"
        platemap$hit <- NA
        
    } else if (scale == "plate"){
        stop("Not added this yet.")
        #         ## need to add scale-by-plate functionality        
        #         # normalised on a plate-by-plate basis
        #         platemap <- as.data.frame(well)
        #         names(platemap)[1] <- "well"
        #         platemap <- mutate(platemap,
        #                            Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
        #                            Column = as.numeric(substr(well, 2, 5)))
        #         values <- as.data.frame(data)
        #         plate_label <- as.data.frame(plate_id)
        #         scaled_data <- values # just raw/un-scaled values for now
        #         platemap <- cbind(platemap, scaled_data[,1], plate_id)
        #         names(platemap)[4] <- "scaled_data"
        #         names(platemap)[5] <- "plate_label"
        #         platemap_temp <- group_by(platemap, plate_label)
        #         platemap_temp2 <- summarise(platemap_temp, scale(scaled_data))
        #         platemap <- as.data.frame(platemap_temp2)
        
        
    } else stop("Not a valid input for 'scale'. \nOptions: 'all' or 'plate'.")
    
    
    # calculate whether values are beyond the threshold; defined as hit or null
    for (row in 1:nrow(platemap)){
        if (scaled_data[row] > threshold){platemap$hit[row] <- "hit"
        } else  if (scaled_data[row] < (-1 * threshold)){platemap$hit[row] <- "neg_hit"
        } else {platemap$hit[row] <- "null"}
    }
    
    # RColorBrewerPallette
    my_cols <- brewer.pal(3, palette)
    my_colours <- c(hit = my_cols[1], neg_hit = my_cols[3], null = my_cols[2])
    
    
    if (plate == 96){
      # produce a 96-well plate map layout in ggplot
      plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
          geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                     color = "grey90", fill = "white", shape = 21, size = 6) +
          geom_point(aes(fill = hit), colour = "gray20", shape = 21, size = 10) +
          coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
          scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
          scale_x_continuous(breaks = seq(1, 12)) +
          ggtitle(title) + 
          scale_fill_manual("hit", values = my_colours) + 
          theme_bw() +
          theme(panel.margin.x = unit(1, "lines"), 
          panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
          facet_wrap(~plate_label,
                     ncol = ncols)
      } else if (plate == 384){
      # produce a 384-well plate map layout in ggplot
      plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
          geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                     color = "grey90", fill = "white", shape = 22, size = 3) +
          geom_point(aes(fill = hit), colour = "gray20", shape = 22, size = 5) +
          coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
          scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
          scale_x_continuous(breaks = seq(1, 24)) +
          ggtitle(title) +
          scale_fill_manual("hit", values = my_colours) + 
          theme_bw() +
          theme(panel.margin.x = unit(1, "lines"), 
          panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
          facet_wrap(~plate_label,
                     ncol = ncols)
    } else stop("Not a valid plate format. Enter either 96 or 384.", call. = FALSE)
  
  return(plt)
}
