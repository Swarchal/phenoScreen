##################################################################################
# plot_confidence
#------------------------------------------------------------------------------
# plots bivariate confidence band calculated via a two-dimensional kernal
# density estimation
# arguments:
#           - 'x': first variable
#           - 'y': second variable
#           - 'confidence': confidence interval between 0:1 at which to draw line
#-------------------------------------------------------------------------------
# e.g:
# x <- rnorm(1000)
# y <- rnorm(1000)
# plot_confidence(x, y, title = "Example plot")
################################################################################

plot_confidence <- function(x, y, confidence = 0.95, title = "", xlab = "x", ylab = "y"){
    
    kerneld <- kde2d(x, y) # kde2d estimate for x and y
    
    pp <- array() # initialise an array
    for (i in 1:1000){ # loop for every element in x or y
        z.x <- max(which(kerneld$x < x[i])) 
        z.y <- max(which(kerneld$y < y[i]))
        pp[i] <- kerneld$z[z.x, z.y]
    }
    
    confidencebound <- quantile(pp,
                                (1 - confidence),
                                na.rm = TRUE)
    
    return(
        plot(x, y,
             pch = 20,
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
