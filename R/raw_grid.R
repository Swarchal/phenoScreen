raw_grid <- function(data, well,
                     plate_id,
                     ncols = 2,
                     plate = 96,
                     title = "",
                     palette = "YlOrRd"){
  
  ## multiple platemap plots in a single figure using facet_wrap
  
  require(dplyr)
  require(ggplot2)
  require(RColorBrewer)
  
  # transform well labels into row-column values for a 96-well plate
  # need to include plate_id labels into this dataframe
  platemap <- as.data.frame(well)
  names(platemap)[1] <- "well"
  platemap <- mutate(platemap,
                     Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                     Column = as.numeric(substr(well, 2, 5)))
  values <- as.data.frame(data)
  plate_label <- as.data.frame(plate_id)
  platemap <- cbind(platemap, values, plate_id)
  names(platemap)[4] <- "raw_data"
  names(platemap)[5] <- "plate_label"
  
  
  # RColorBrewerPallette settings
  my_cols <- brewer.pal(9, palette)
  
  # produce a plate map in ggplot (96-well format)
  plt96 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
               color = "grey90", fill = "white", shape = 21, size = 6) +
    geom_point(aes(fill = raw_data), colour = "gray20", shape = 21, size = 10) +
    coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
    scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
    scale_x_continuous(breaks = seq(1, 12)) +
    scale_fill_gradient2("values",
                         low  = my_cols[1],
                         mid  = my_cols[4],
                         high = my_cols[9]) +
    ggtitle(title) +
    theme_bw() + 
    facet_wrap(~plate_label,
               ncol = ncols)
  
  # produce a plate map in ggplot (384-well format)
  plt384 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
               color = "grey90", fill = "white", shape = 22, size = 3) +
    geom_point(aes(fill = raw_data), colour = "gray20", shape = 22, size = 5) +
    coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
    scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
    scale_x_continuous(breaks = seq(1, 24)) +
    scale_fill_gradient2("values",
                         low  = my_cols[1],
                         mid  = my_cols[4],
                         high = my_cols[9]) +
    ggtitle(title) +
    theme_bw() +
    facet_wrap(~plate_label,
               ncol = ncols)
  
  if (plate == 96)  {return(plt96)}
  if (plate == 384) {return(plt384)}
  else stop("Invalid argument for 'plate'. \nOption: 96 or 384.",
            call. = FALSE)
  
}