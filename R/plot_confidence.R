#' Bivariate scatter plot with a confidence boundary
#' 
#' Bivariate scatter plot with a confidence boundary calculates via a 
#' 2-dimensional kernel density estimate
#' 
#' @param x Numerical vector
#' @param y Numerical vector
#' @param confidence Confidence interval between 0 and 1, default is 0.9 i.e 90\% CI
#' @param title Title of plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' 
#' @importFrom MASS kde2d
#'
#' @return plot
#'
#' @export
#' 
#' @examples
#' x <- rnorm(1000)
#' y <- rnorm(1000)
#' plot_confidence(x, y, title = "Example plot")

plot_confidence <- function(x, y,
                            confidence = 0.9,
                            title = "",
                            xlab = "x",
                            ylab = "y"){
    
    kerneld <- kde2d(x, y) # kde2d estimate for x and y
    
    pp <- array() # initialise an array
    for (i in 1:length(x)){ # loop for every element in x or y
        z.x <- max(which(kerneld$x < x[i])) 
        z.y <- max(which(kerneld$y < y[i]))
        pp[i] <- kerneld$z[z.x, z.y]
    }
    
    confidencebound <- quantile(pp,
                                (1 - confidence),
                                na.rm = TRUE)
    
    return(
        plot(x, y,
             cex = 0.5,
             main = title,
             xlab = xlab,
             ylab = ylab) + 
        contour(kerneld,
                levels = confidencebound,
                col = "red",
                add = TRUE,
                drawlabels = FALSE)
    )
}
