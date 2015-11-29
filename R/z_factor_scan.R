#' Multiple z-factor calculations
#' 
#' Calculates z-factors between two compounds for multiple variables and returns the
#' z-factors and associated features above a specified cut-off. The cut-off can be
#' determined either by a minumum z-factor score, or return the top n features.
#' 
#' @param data Dataframe, containing only numerical columns of features and
#' 		single column of treatment labels
#' @param treatments Vector of factors of treatment groups under the column of 'header'
#' @param cutoff Minimum z-factor threshold for returning featues. Default is 0.5
#' @param plot If TRUE, will plot features and z-factor scores
#' @param title Title of plot
#' @param plotline z-factor at which to draw line if plot is TRUE
#' @param ylabel y-axis label if plot is TRUE
#' @param n Highest n features to be returned
#' 
#' @return Z-factors or plots result of plot is TRUE


z_factor_scan <- function(data, treatments, cutoff = 0.5, plot = FALSE, title = "", plotline = 0.5, ylabel = "Feature", n = FALSE){

		# function to calculate z-factor when given two tidy vectors
		#------------------------------------------------------------------------------------------
		z_factor <- function(positive, negative){

		# calculates the z-factor between selected upper and lower bound groups
		# a value between 0.5 and 1 is considered robust

		positive <- as.vector(positive)
		negative <- as.vector(negative)

		mu_p <- mean(positive, na.rm = TRUE)
		mu_n <- mean(negative, na.rm = TRUE)    

		  # idiot proof error handling for input order
		  # assigns input with greatest mean as upper group, smaller mean -> lower group
		if (mu_n > mu_p){
		  mu_p <- mean(negative, na.rm = TRUE)
		  mu_n <- mean(positive, na.rm = TRUE)
		}

		sigma_p <- sd(positive)
		sigma_n <- sd(negative)
        
        # calculate z-factor
		z_prime <- 1 - ((3*(sigma_p + sigma_n)) / (mu_p - mu_n))
		return(z_prime)
		}
		#------------------------------------------------------------------------------------------

	header_in <- which(colnames(data) == "header") # index of column named 'header'
	nums <- (1:dim(data)[2])[ - header_in] # indices of all columns except header
	z_values <- c() # initialise for the loop
	z_names <- c() # initialise for the loop

	for (number in nums){ # for each feature (except header)
	z_values[number] <- (as.vector(
        z_factor(subset(data[,number], data$header == treatments[1]),
                 subset(data[,number], data$header == treatments[2])))
        )
	z_names[number] <- names(data)[number]
	}
    
    # default behaviour, returns features and values above specified cutoff
	if (n == FALSE){
	    z_factors <- data.frame(z_names, z_values) # create dataframe
	    # subset of z_factors above cutoff
	    z_factors_good <- subset(z_factors, z_factors$z_values > cutoff)
	    # names for dataframe columns
	    names(z_factors_good)[c(1,2)] <- c("Feature", "Z_factor")
	    # order dataframe from highest z-factor to the lowest
	    z_out <- z_factors_good[with(z_factors_good, order(-Z_factor)), ]
	}
    
    # The 'n' argument is return the highest 'n' values for Z-factor
    # Default behavious is n = FALSE, so instead calls the cutoff value.
    if (is.logical(n) == FALSE){
        if (n == TRUE) stop("n has to be either FALSE or an integer", call. = FALSE)
        n <- as.integer(n) # forces n into an integer
        z_factors <- data.frame(z_names, z_values) # create dataframe
        names(z_factors)[c(1,2)] <- c("Feature", "Z_factor") # assign colnames
        z_factors_order <- z_factors[order(- z_factors$Z_factor), ] # re-order
        z_out <- z_factors_order[1:n, ] # subset first n values
    }
	
    # plot:
	if (plot == TRUE){
	    require(ggplot2)
	    
        # order factors dependent on Z-factor score makes it so features in
        # plot go from highest to lowest rather than in alphabetical order
	    z_out2=z_out[order(z_out$Z_factor),]
	    z_out2$Feature=factor(z_out2$Feature,levels=z_out2$Feature)
	    
        # ggplot dotchart
	    plt <- ggplot(data = z_out2, aes(x = Z_factor, y = Feature)) +
	        geom_point(colour = "gray30", pch = 19) +
	        geom_vline(xintercept = plotline, col = "gray50", linetype = 2) +
	        theme_bw() +
	        theme(legend.position = "none") + 
	        ggtitle(title) +
	        xlab("Z factor") +
            ylab(ylabel)
	    return(plt)
	}
	return(z_out)
}