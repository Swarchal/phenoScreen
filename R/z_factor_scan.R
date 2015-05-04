###################################################################################################
# z_factor_scan
#--------------------------------------------------------------------------------------------------
# calculates z-factors between two compounds for multiple variables
# returns the z-factors and associated variables above a specified cut off: default = 0.5
###################################################################################################
#############   N.B: treatment groups must be under the column label 'header'   ###################
###################################################################################################
# * argument: 'data', dataset to be used, containing only numerical columns of variables, and single
# column containing treatment labels
# * argument: `treatments`, vector containing treatment groups
# recommended to be positive and negative controls
#  * argument: 'cutoff', numerical, selects the minimum value of returned z-factors
#--------------------------------------------------------------------------------------------------
# e.g:
# z_factor_scan(data = tidy_dataset, 
#				treatments = c("DMSO", "STS"),
#				cutoff = 0.3)
# e.g 2:
# cntrols <- c("DMSO", "STS")
# z_factor_scan(tidy_dataset, cntrols, 0.3)
#--------------------------------------------------------------------------------------------------
# TODO: option for selecting column containing treatment lablels,
# at the moment column *has* to be named 'header'
###################################################################################################



z_factor_scan <- function(data, treatments, cutoff = 0.5, plot = FALSE, title = "", plotline = 0.5, ylabel = "Feature"){

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

	z_factors <- data.frame(z_names, z_values) # create dataframe
    # subset of z_factors above cutoff
	z_factors_good <- subset(z_factors, z_factors$z_values > cutoff)
    # names for dataframe columns
	names(z_factors_good)[c(1,2)] <- c("Feature", "Z_factor")
    # order dataframe from highest z-factor to the lowest
	z_out <- z_factors_good[with(z_factors_good, order(-Z_factor)), ]
	
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