plot_confidence <- function(x, y,
                            confidence = 0.9,
                            title = "",
                            xlab = "x",
                            ylab = "y"){
    require(MASS)
    
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
