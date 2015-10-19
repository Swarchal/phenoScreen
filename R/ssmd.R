ssmd <- function(a, b,
                 verbose = TRUE){
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
    
    
    if (verbose == TRUE){
        diff <- abs(beta)
        if (diff == 0) message("No effect")
        if (diff <= 0.25 & diff > 0) message("Extremely weak")
        if (diff <= 0.5 & diff > 0.25) message("Very weak")
        if (diff <= 0.75 & diff > 0.5) message("Weak")
        if (diff <= 1 & diff > 0.75) message("Fairly weak")
        if (diff <= 1.28 & diff > 1) message("Fairly moderate")
        if (diff <= 1.654 & diff > 1.28) message("Moderate")
        if (diff <= 2 & diff > 1.654) message("Fairly strong")
        if (diff <= 3 & diff > 2) message("Strong")
        if (diff <= 5 & diff > 3) message("Very strong")
        if (diff >= 5) message("Extremely strong (yay)")
    }
    
    return(beta)
}