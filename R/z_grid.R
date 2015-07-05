z_grid <- function(data, well,
                   plate_id,
                   ncols = 2,
                   plate = 96,
                   title = "",
                   scale = "all",
                   palette = "Spectral"){
    
    ## multiple platemap plots in a single figure using facet_wrap
    
    require(dplyr)
    require(ggplot2)
    require(RColorBrewer)
    
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
    
    
    # RColorBrewerPallette
    my_cols <- brewer.pal(3, palette)
    
    # produce a plate map in ggplot (96-well format)
    plt_96 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
        geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                   color = "grey90", fill = "white", shape = 21, size = 6) +
        geom_point(aes(fill = scaled_data), colour = "gray20", shape = 21, size = 10) +
        coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
        scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
        scale_x_continuous(breaks = seq(1, 12)) +
        scale_fill_gradient2("z-score",
                             low = my_cols[3],
                             high = my_cols[1],
                             mid = my_cols[2]) +
        ggtitle(title)+
        theme_bw() + 
        facet_wrap(~plate_label,
                   ncol = ncols)
    
    # produce a plate map in ggplot (384-well format)
    plt_384 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
        geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                   color = "grey90", fill = "white", shape = 22, size = 3) +
        geom_point(aes(fill = scaled_data), colour = "gray20", shape = 22, size = 5) +
        coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
        scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
        scale_x_continuous(breaks = seq(1, 24)) +
        scale_fill_gradient2("z-score",
                             low = my_cols[3],
                             high = my_cols[1],
                             mid = my_cols[2]) +
        ggtitle(title) +
        theme_bw() +
        facet_wrap(~plate_label,
                   ncol = ncols)
    
    if (plate == 96)  {return(plt_96)}
    if (plate == 384) {return(plt_384)
    } else stop("Not a valid plate format. Enter either 96 or 384.", call. = FALSE)
    
    
}
