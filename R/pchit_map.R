pchit_map <- function(data, well,
                      plate = 96,
                      threshold = 2,
                      title = "",
                      palette = "Spectral"){
    
    require(ggplot2)
    require(dplyr)
    require(RColorBrewer)
    
    pca_data <- prcomp(data) # pca of data
    pc1 <- pca_data$x[,1] # take first principal component
    
    pc_hit_map <- hit_map(
        pc1,
        well,
        plate,
        threshold,
        title,
        palette)

    return(pc_hit_map)

}