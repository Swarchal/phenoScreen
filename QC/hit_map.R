###############################################################################
# hit_map
#------------------------------------------------------------------------------
# calculates z-score values for a 96 or 384-well microtitre plate and plots
# the results as three colours dependent on z-score threshold to visualise hits
#
# arguments:
#   - 'values':     column of numerical values with which to calculate z-score
#   - 'platemap':   column of well ID's
#   - 'plate':      number of wells in plate (96[default] or 364)
#   - 'threshold':  value of z-score to determine hit/not-hit
#   - 'title':      self-explanatory
#   - 'palette':    RColorBrewer palette for heatmap colours
#
# e.g:
# hit_map(values = df$vals,
#         platemap = df$well,
#         plate = 384)
###############################################################################

hit_map <- function(values, platemap, plate = 96, threshold = 2, title = "", palette = "Spectral"){
    
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
    platemap$hit <- NA
    
    # calculate whether values are beyond the threshold; defined as hit or null
    for (row in 1:nrow(platemap)){
        if (scaled_data[row] > threshold){platemap$hit[row] <- "hit"
        } else  if (scaled_data[row] < (-1 * threshold)){platemap$hit[row] <- "neg_hit"
        } else {platemap$hit[row] <- "null"}
    }
    
    # RColorBrewerPallette
    my_cols <- brewer.pal(3, palette)
    my_colours <- c(hit = my_cols[1], neg_hit = my_cols[3], null = my_cols[2])
    
    # produce a 96-well plate map layout in ggplot
    plt_96 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
        geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                   color = "grey90", fill = "white", shape = 21, size = 6) +
        geom_point(aes(fill = hit), colour = "gray20", shape = 21, size = 10) +
        coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
        scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
        scale_x_continuous(breaks = seq(1, 12)) +
        ggtitle(title) + 
        scale_fill_manual("hit", values = my_colours) + 
        theme_bw()

    # produce a 384-well plate map layout in ggplot
    plt_384 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
        geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                   color = "grey90", fill = "white", shape = 22, size = 3) +
        geom_point(aes(fill = hit), colour = "gray20", shape = 22, size = 5) +
        coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
        scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
        scale_x_continuous(breaks = seq(1, 24)) +
        ggtitle(title) +
        scale_fill_manual("hit", values = my_colours) + 
        theme_bw()

    if (plate == 96) {return(plt_96)}
    if (plate == 384) {return(plt_384)
    } else stop("Not a valid plate format. Either 96 or 384.", call. = FALSE)

}