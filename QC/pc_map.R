###############################################################################
# pc_map
#-----------------------------------------------------------------------------
# plots a plate heatmap of the first principal component
#
# arguments:
#           - 'data': columsn of numerical values to pass to princomp()
#           - 'well': column of well identifying labels
###############################################################################

pc_map <- function(data, well, title = "", palette = "Spectral"){
    
    pca_data <- princomp(data) # pca of data
    pc1 <- pca_data$score[,1] # take first principal component
    
    z_map <- function(values, platemap, title = "", palette = "Spectral"){
        
        require(ggplot2)
        require(dplyr)
        require(RColorBrewer)
        
        # transform well labels into row-column values for a 96-well plate
        platemap <- as.data.frame(platemap)
        names(platemap)[1] <- "well"
        platemap <- mutate(platemap,
                           Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                           Column = as.numeric(substr(well, 2, 5)))
        
        values <- as.data.frame(values)
        scaled_data <- scale(values)
        platemap <- cbind(platemap, scaled_data[,1])
        names(platemap)[4] <- "scaled_data"
        
        # RColorBrewerPallette
        my_cols <- brewer.pal(3, palette)
        
        # produce a plate map in ggplot
        plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
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
        return(plt)
    }
    
    plot_pc_map <- z_map(pc1, well, title, palette)
    return(plot_pc_map)
    
}