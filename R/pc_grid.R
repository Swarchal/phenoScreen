pc_grid <- function(data, well,
                    plate_id,
                    ncols = 2,
                    plate = 96,
                    title = "",
                    palette = "Spectral"){
                    
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  require(grid)
  
  pca_data <- prcomp(data) # pca of data
  pc1 <- pca_data$x[, 1] # take first principal component
  
  pc_grid <- z_grid(pc1,
    well,
    plate_id,
    ncols,
    plate,
    title,
    palette)

  return(pc_grid)
}
