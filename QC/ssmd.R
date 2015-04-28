#######################################################
# ssmd()
# Strictly standardised mean difference
#------------------------------------------------------
# a measure of effect size between two compounds
# i.e compound and (positive) control
#######################################################

ssmd <- function(a, b){

	if(is.numeric(c(a, b) == FALSE)){
		stop("Requires numerical values")
	}
	
	mu_1 <- mean(a, na.rm = TRUE)
	mu_2 <- mean(b, na.rm = TRUE)

	beta = (mu_1 - mu_2)/sqrt(sd(a) + sd(b) - 2*cov(a, b))
	
	return(beta)
}