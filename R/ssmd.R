ssmd <- function(a, b){
    # check input
    if (length(a) < 2 | length(b) < 2){
        stop(call. = FALSE,
             "Inputs need to be greater at least 2 elements long")
    }
    if (is.numeric(a) == FALSE | is.numeric(b) == FALSE){
        stop(call. = FALSE,
             "Input needs to be numeric.")
    }
    
    mu_a <- mean(a, na.rm = TRUE)
    mu_b <- mean(b, na.rm = TRUE)
    
    var_a <- var(a, na.rm = TRUE)
    var_b <- var(b, na.rm = TRUE)
    
    cov_ab <- cov(a, b)
    
    beta <- (mu_a - mu_b) / sqrt(var_a + var_b - 2 * cov_ab)
    return(beta)
}