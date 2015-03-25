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



z_factor_scan <- function(data, treatments, cutoff = 0.5){

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

		z_prime <- 1 - ((3*(sigma_p + sigma_n)) / (mu_p - mu_n))
		return(z_prime)
		}
		#------------------------------------------------------------------------------------------

	header_in <- which(colnames(data) == "header") # index of column named 'header'
	nums <- (1:dim(data)[2])[ - header_in] # indices of all columns except header
	z_values <- c()
	z_names <- c()

	for (number in nums){
	z_values[number] <- (as.vector(z_factor(subset(data[,number], data$header == treatments[1]),
	                            subset(data[,number], data$header == treatments[2]))))
	z_names[number] <- names(data)[number]
	}

	z_factors <- data.frame(z_names, z_values)
	z_factors_good <- subset(z_factors, z_factors$z_values > cutoff)
	names(z_factors_good)[c(1,2)] <- c("Feature", "Z_factor")
	return(z_factors_good[with(z_factors_good, order(-Z_factor)), ])

}