pchit_grid <- function(data, well,
                      plate_id,
                      ncols = 2,
                      plate = 96,
                      threshold = 2,
                      title = "",
                      palette = "Spectral"){
  
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  
  pca_data <- prcomp(data) # pca of data
  pc1 <- pca_data$x[,1] # take first principal component
  
  pc_hit_grid <- hit_grid(
    pc1,
    well,
    plate_id,
    ncols,
    plate,
    threshold,
    title,
    palette)

  return(pc_hit_grid)
  
}