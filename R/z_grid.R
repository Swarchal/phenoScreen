z_grid <- function(data, well,
                   plate_id,
                   ncols = 2,
                   plate = 96,
                   title = "",
                   palette = "Spectral"){
  
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  require(grid)
  
  # transform well labels into row-column values
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
  
  if (plate == 96){
    plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
      geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                 color = "grey90", fill = "white", shape = 21, size = 6) +
      geom_point(aes(fill = scaled_data), colour = "gray20", shape = 21, size = 10) +
      coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
      scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
      scale_x_continuous(breaks = seq(1, 12)) +
      scale_fill_distiller("z-score", palette = palette) +
      ggtitle(title) +
      theme_bw() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols)
  } else if (plate == 384){
    plt <- ggplot(data = platemap, aes(x = Column, y = Row)) +
      geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                 color = "grey90", fill = "white", shape = 22, size = 3) +
      geom_point(aes(fill = scaled_data), colour = "gray20", shape = 22, size = 5) +
      coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.25, 24.75), ylim = c(0.3, 16.7)) +
      scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
      scale_x_continuous(breaks = seq(1, 24)) +
      scale_fill_gradient2("z-score", palette = palette) +
      ggtitle(title) +
      theme_bw() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(1, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols)
  } else stop("Invalid argument for 'plate'. \nOptions: 96 or 384.",
            call. = FALSE)

  return(plt)
}