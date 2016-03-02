#' Plots multiple platemaps with heatmap of scaled values
#' 
#' Converts numerical values. well labels, and plate labels into multiple 
#' plate heatmaps
#' 
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @param ncols Number of columns to display multiple heatmaps
#' @param plate Number of wells in complete plate (96 or 384)
#' @param each boolean, if true scales each plate individually, if false will
#'     scale the pooled values of \code{data}
#' @param title Title of plot
#' @param palette RColorBrewer palette
#' 
#' @return ggplot plot
#'
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#'
#' @export
#'
#' @examples
#' df01 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 1)
#' 
#' df02 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 2)
#' 
#' df <- rbind(df01, df02)
#' 
#' z_grid(data = df$vals,
#'        well = df$well,
#'        plate_id = df$plate,
#'        plate = 96,
#'        title = "Plot Title")

z_grid <- function(data, well,
                   plate_id,
                   ncols = 2,
                   plate = 96,
                   title = "",
                   palette = "Spectral",
                   each = FALSE){

  stopifnot(is.vector(data))
  
  # transform well labels into row-column values
  platemap <- plate_map_grid_scale(data, well, plate_id, each)
  
  if (plate == 96){
    plt <- plt96(platemap) +
      scale_fill_distiller("z-score", palette = palette) +
      ggtitle(title) +
      theme_bw() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols,
                 scales = 'free')
  } else if (plate == 384){
    plt <- plt384(platemap) +
      scale_fill_distiller("z-score", palette = palette) +
      ggtitle(title) +
      theme_bw() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(1, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols,
                 scales = 'free')
  } else stop("Invalid argument for 'plate'. \nOptions: 96 or 384.",
            call. = FALSE)

  return(plt)
}





#' Z_grid, but prettier
#' 
#' z_grid using ggplot dark theme and viridis colour map
#'
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @param ncols Number of columns to display multiple heatmaps
#' @param plate Number of wells in complete plate (96 or 384)
#' @param each boolean, if true scales each plate individually, if false will
#'     scale the pooled values of \code{data}
#' @param title Title of plot
#' @param option viridis colour option, 'A', 'B', 'C', or 'D'
#' 
#' @import ggplot2
#' @import dplyr
#' @import viridis
#' 
#' @export
#' @examples
#' df01 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 1)
#' 
#' df02 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 2)
#' 
#' df <- rbind(df01, df02)
#' 
#' z_grid__(data = df$vals,
#'        well = df$well,
#'        plate_id = df$plate,
#'        plate = 96,
#'        title = "Plot Title")

z_grid__ <- function(data, well,
                   plate_id,
                   ncols = 2,
                   plate = 96,
                   title = "",
                   option = "D",
                   each = FALSE){

  stopifnot(is.vector(data))
  
  # transform well labels into row-column values
  platemap <- plate_map_grid_scale(data, well, plate_id, each)
  
  if (plate == 96){
    plt <- plt96(platemap) +
      scale_fill_viridis("z-score", option = option) +
      ggtitle(title) +
      theme_dark() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols,
                 scales = 'free')
  } else if (plate == 384){
    plt <- plt384(platemap) +
      scale_fill_viridis("z-score", option = option) +
      ggtitle(title) +
      theme_dark() +
      theme(panel.margin.x = unit(1, "lines"), 
            panel.margin.y = unit(1, "lines")) + # increase spacing between facets
      facet_wrap(~plate_label,
                 ncol = ncols,
                 scales = 'free')
  } else stop("Invalid argument for 'plate'. \nOptions: 96 or 384.",
            call. = FALSE)

  return(plt)
}
