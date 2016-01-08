#' Plots distributions per well in a plate layout
#'
#' Produces distribution plots facetted in a plate-layout format. Requires
#' columns within a dataframe labelled 'row' and 'column' indicating the plate
#' row and column, these can be produced with \code{num_to_well}.
#'
#' @param data Dataframe
#' @param feature Numerical values indicating column index which contains the values
#'    to produce the distribution.
#' @param row Number indicating the row within a plate
#' @param column Number inficating the column within a plate
#' @param title Title of the plot
#'
#' @export
#'
#' @return ggplot plot


dist_map <- function(data, feature, row, column, title = ""){
    
    require(ggplot2)
    
    localenv <- environment()
    
    plt <- ggplot(data = data,
                  aes(x = data[,feature]),
                  environment = localenv) +
        geom_density(alpha = 0.6,
                     fill = "gray80") + 
        facet_grid(row ~ column) + 
        xlab(colnames(data)[feature]) + 
        theme_bw() + 
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) # rot. x-axis lab
    
    return(plt)
}
