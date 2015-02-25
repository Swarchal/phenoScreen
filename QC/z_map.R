###############################################################################
# z_map
#------------------------------------------------------------------------------
# calculates z-score values for a 96-well microtitre plate and plots
# the results as a heatmap in plate format
#
# currently:
#   calculates z-score on a well basis dependent on the plate mean
###############################################################################

z_map <- function(data, platemap){
    
    require(ggplot2)
    require(dplyr)
    
    # calculates CV values
    z_data <- scale(data)
    
    # transform well labels into row-column values for a 96-well plate
    platemap <- mutate(platemap,
                       Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                       Column=as.numeric(substr(Well, 2, 5)))
    
    # produce a plate map in ggplot
    plt <- ggplot(data=platemap, aes(x=Column, y=Row)) +
        geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2),
                   color="grey90", fill="white", shape=21, size=6) +
        geom_point(size=10, colour = z_data[,1]) + # CV as continuous colour
        coord_fixed(ratio=(13/12)/(9/8), xlim=c(0.5, 12.5), ylim=c(0.5, 8.5)) +
        scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) +
        scale_x_continuous(breaks=seq(1, 12)) +
        labs(title="Plate Layout for My Experiment")
    return(plt)
}