pc_map <- function(data, well,
    plate = 96,
    title = "",
    palette = "Spectral"){
    
    pca_data <- prcomp(data) # pca of data
    pc1 <- pca_data$x[,1] # take first principal component
    
    plot_pc_map <- z_map(pc1, well, plate, title, palette)
    return(plot_pc_map)
     
}
