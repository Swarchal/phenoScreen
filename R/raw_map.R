raw_map <- function(data, well,
                    plate = 96,
                    title = "",
                    palette = "YlGnBu"){
  
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  
  # transform well labels into row-column values
  platemap <- as.data.frame(well)
  names(platemap)[1] <- "well"
  platemap <- mutate(platemap,
                     Row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                     Column = as.numeric(substr(well, 2, 5)))
  
  platemap <- cbind(platemap, data)
  names(platemap)[4] <- "raw_data"
  
  plt96 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
               color = "grey90", fill = "white", shape = 21, size = 6) +
    geom_point(aes(fill = raw_data), colour = "gray20", shape = 21, size = 10) +
    coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
    scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
    scale_x_continuous(breaks = seq(1, 12)) +
    scale_fill_gradient2("values", palette = palette) +
    ggtitle(title) +
    theme_bw()
  
  plt384 <- ggplot(data = platemap, aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
               color = "grey90", fill = "white", shape = 22, size = 3) +
    geom_point(aes(fill = raw_data), colour = "gray20", shape = 22, size = 5) +
    coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
    scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
    scale_x_continuous(breaks = seq(1, 24)) +
    scale_fill_gradient2("values", palette = palette) +
    ggtitle(title) +
    theme_bw()

  if (plate == 96)  {return(plt96)}
  if (plate == 384) {return(plt384)}
  else stop("Invalid argument for 'plate'. \nOption: 96 or 384",
            call. = FALSE)
  
}