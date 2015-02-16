#--------------------------------------------------------------
# Z factor function
#--------------------------------------------------------------


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

