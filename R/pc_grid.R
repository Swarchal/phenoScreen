pc_grid <- function(data, well,
                    plate_id,
                    ncols = 2,
                    plate = 96,
                    title = "",
                    palette = "Spectral"){
  
  pca_data <- prcomp(data) # pca of data
  pc1 <- pca_data$x[, 1] # take first principal component
  
  # define z_map with multiple plots via facet_wrap
  
  # transform well labels into row-column values for a 96-well plate
  platemap <- as.data.frame(well)
  names(platemap)[1] <- "well"
  platemap <- mutate(platemap,
                     Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                     Column = as.numeric(substr(well, 2, 5)))
  values <- as.data.frame(pc1)
  plate_label <- as.data.frame(plate_id)
  scaled_data <- scale(values)
  platemap <- cbind(platemap, scaled_data[,1], plate_id)
  names(platemap)[4] <- "scaled_data"
  names(platemap)[5] <- "plate_label"
  
  # RColorBrewerPallette
  my_cols <- brewer.pal(3, palette)
  if (plate == 96){  
    # produce a plate map in ggplot
    plt_96 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
      geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                 color = "grey90", fill = "white", shape = 21, size = 6) +
      geom_point(aes(fill = scaled_data), colour = "gray20", shape = 21, size = 10) +
      coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
      scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
      scale_x_continuous(breaks = seq(1, 12)) +
      scale_fill_gradient2("z-score",
                           low  = my_cols[3],
                           mid  = my_cols[2],
                           high = my_cols[1]) +
      ggtitle(title) +
      theme_bw() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(1, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols)
    
    return(plt_96)
  }
  
  if (plate == 384){
    plt_384 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
      geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                 color = "grey90", fill = "white", shape = 22, size = 3) +
      geom_point(aes(fill = scaled_data), colour = "gray20", shape = 22, size = 5) +
      coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
      scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
      scale_x_continuous(breaks = seq(1, 24)) +
      scale_fill_gradient2("z-score",
                           low  = my_cols[3],
                           mid  = my_cols[2],
                           high = my_cols[1]) +
      ggtitle(title) +
      theme_bw() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(1, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols)
    
    return(plt_384)
  }
  else stop("Invalid argument for plate. \nOption: 96 or 384.")
}