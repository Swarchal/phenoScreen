plot_rose <- function(data,
                      magnitude,
                      theta,
                      magnituderes = 2,
                      thetares = 30,
                      magnitudemin = 2,
                      magnitudemax = 20,
                      magnitudeseq = NULL,
                      palette = "YlGnBu",
                      title = "",
                      xlab = "",
                      ylab = "",
                      legend_title = "magnitude",
                      countmax = NA,
                      debug = 0){
    
    require(ggplot2)
    require(RColorBrewer)
    
    # Look to see what data was passed in to the function
    if (is.numeric(magnitude) & is.numeric(theta)){
        # assume that we've been given vectors of the magnitude and theta vectors
        data <- data.frame(magnitude = magnitude,
                           theta = theta)
        magnitude = "magnitude"
        theta = "theta"
    } else if (exists("data")){
        # Assume that we've been given a data frame, and the name of the magnitude 
        # and theta columns. This is the format we want for later use.    
    }  
    
    # Tidy up input data ----
    n.in <- NROW(data)
    dnu <- (is.na(data[[magnitude]]) | is.na(data[[theta]]))
    data[[magnitude]][dnu] <- NA
    data[[theta]][dnu] <- NA
    
    # figure out the magnitude bins ----
    if (missing(magnitudeseq)){
        magnitudeseq <- seq(magnitudemin,magnitudemax,magnituderes)
    } else {
        if (debug >0){
            cat("Using custom magnitude bins \n")
        }
    }
    # get some information about the number of bins, etc.
    n.magnitude.seq <- length(magnitudeseq)
    n.colors.in.range <- n.magnitude.seq - 1
    
    # create the color map
    magnitude.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                      n.colors.in.range),
                                                  min(9,
                                                      n.colors.in.range)),                                               
                                              palette))(n.colors.in.range)
    
    if (max(data[[magnitude]],na.rm = TRUE) > magnitudemax){    
        magnitude.breaks <- c(magnitudeseq,
                        max(data[[magnitude]],na.rm = TRUE))
        magnitude.labels <- c(paste(c(magnitudeseq[1:n.magnitude.seq-1]),
                              '-',
                              c(magnitudeseq[2:n.magnitude.seq])),
                        paste(magnitudemax,
                              "-",
                              max(data[[magnitude]],na.rm = TRUE)))
        magnitude.colors <- c(magnitude.colors, "grey50")
    } else{
        magnitude.breaks <- magnitudeseq
        magnitude.labels <- paste(c(magnitudeseq[1:n.magnitude.seq-1]),
                            '-',
                            c(magnitudeseq[2:n.magnitude.seq]))    
    }
    data$magnitude.binned <- cut(x = data[[magnitude]],
                           breaks = magnitude.breaks,
                           labels = magnitude.labels,
                           ordered_result = TRUE)
    
    # figure out the theta bins
    theta.breaks <- c(-thetares/2,
                    seq(thetares/2, 360-thetares/2, by = thetares),
                    360+thetares/2)  
    theta.labels <- c(paste(360-thetares/2,"-",thetares/2),
                    paste(seq(thetares/2, 360-3*thetares/2, by = thetares),
                          "-",
                          seq(3*thetares/2, 360-thetares/2, by = thetares)),
                    paste(360-thetares/2,"-",thetares/2))
    # assign each theta to a bin
    theta.binned <- cut(data[[theta]],
                      breaks = theta.breaks,
                      ordered_result = TRUE)
    levels(theta.binned) <- theta.labels
    data$theta.binned <- theta.binned
    
    # Run debug if required ----
    if (debug>0){    
        cat(theta.breaks,"\n")
        cat(theta.labels,"\n")
        cat(levels(theta.binned),"\n")
        cat(magnitudecuts.colors, "\n")    
    }  
    
    # create the plot ----
    p.rose <- ggplot(data = data,
                         aes(x = theta.binned,
                             fill = magnitude.binned)) +
        geom_bar() + 
        scale_x_discrete(drop = FALSE,
                         labels = waiver()) +
        coord_polar(start = -((thetares/2)/360) * 2*pi) +
        scale_fill_manual(name = legend_title, 
                          values = magnitude.colors,
                          drop = FALSE) +
        theme(axis.title.x = element_blank()) +
        xlab(xlab) + 
        ylab(ylab) +
        ggtitle(title)
    
    # adjust axes if required
    if (!is.na(countmax)){
        p.rose <- p.rose +
            ylim(c(0,countmax))
    }
    
    # print the plot
    print(p.rose)  
    
    # return the handle to the wind rose
    return(p.rose)
}