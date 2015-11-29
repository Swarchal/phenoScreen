#' Calculates area contained within 2D KDE confidence boundary
#'
#' Produces a confidence boudnary wth a 2D kernel density estimate from a bivariate scatter plot,
#' and then calculates the area within the confidence boundary.
#'
#' @param x Values to be used on the x-axis
#' @param y Values to be used on the y-axis
#' @param confidence Confidence interval between 0 and 1
#'      Default is 0.9, i.e 90\% confidence inverval
#'
#' @return The area within the calculated confidence boundary
#'
#' @examples
#' x <- rnorm(1000) ; y <- rnorm(1000)
#' area_of_confidence(x, y)

area_of_confidence <- function(x, y,
                               confidence = 0.9){
    
    require(MASS)
    
    # 2D KDE for x,y co-ordinates
    kerneld <- MASS::kde2d(x, y)
    
    # array of maximum density values for each x,y co-ordinate
    pp <- array()
    for (i in 1:length(x)) {
        z.x <- max(which(kerneld$x < x[i]))
        z.y <- max(which(kerneld$y < y[i]))
        pp[i] <- kerneld$z[z.x, z.y]
    }
    
    # select quantile associated with confidence interval
    confidencebound <- quantile(pp,
                                (1 - confidence),
                                na.rm = TRUE)
    
    # produce a contour line from KDE and confidence quantiles
    out_line <- contourLines(kerneld,
                             levels = confidencebound)
    OL_x <- out_line[[1]]$x
    OL_y <- out_line[[1]]$y
    
    # calculate area of within confidence
    area = 0.5* abs(
        sum(
            OL_x[1:(length(OL_x)-1)] * OL_y[2:length(OL_x)] -
                OL_y[1:(length(OL_x)-1)] * OL_x[2:length(OL_x)]
        )
    )
    return(area)
}

