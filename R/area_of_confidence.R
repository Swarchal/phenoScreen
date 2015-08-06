area_of_confidence <- function(x, y, confidence = 0.9){
    
    require(MASS)
    
    # 2D KDE for x,y co-ordinates
    kerneld <- kde2d(x, y)
    
    # array of maximum density values for each x,y co-ordinate
    pp <- array()
    for (i in 1:length(x)) {
        z.x <- max(which(kerneld$x < x[i]))
        z.y <- max(which(kerneld$y < y[i]))
        pp[i] <- kerneld$z[z.x, z.y]
    }
    
    # select quantile associated with confidence interval
    confidencebound <- quantile(pp, (1 - confidence), na.rm = TRUE)
    
    # produce a contour line from KDE and confidence quantiles
    out_line <- contourLines(kerneld, levels = confidencebound)
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

