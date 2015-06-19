z_map <- function(data, well,
    plate = 96,
    title = "",
    palette = "Spectral"){
    
    require(ggplot2)
    require(dplyr)
    require(RColorBrewer)
    
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
        theme_bw()
    
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
        theme_bw()

    if (length(well) > plate) {
        stop("Invalid plate selection. The data given has more rows than number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is set to a 96-well plate.",
            call. = FALSE)
    }
    if (plate == 96)  {return(plt_96)}
    if (plate == 384) {return(plt_384)
    } else stop("Not a valid plate format. Enter either 96 or 384.", call. = FALSE)
}
