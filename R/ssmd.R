#' Strictly standardised mean difference (SSMD)
#' 
#' SSMD is a measure of effect size, defined at the mean divided by the
#' standard deviation of a difference between two random values each from
#' one of the two groups.
#' \deqn{\frac{\beta = \mu_a - \mu_b}{\sqrt{\sigma_a^2 + \sigma_b^2 - \sigma_{ab}}}}
#' Beta is defined as the ratio of mean to SD of the two groups.
#' 
#' @param a Vector
#' @param b Vector
#' @param verbose if TRUE will return a description of effect size
#' 
#' @return SSMD and effect description if verbose is TRUE
#'
#' @export
#' 
#' @examples
#' a <- rnorm(100, 1)
#' b <- rnorm(100, 10)
#' ssmd(a, b)



ssmd <- function (a, b, verbose = TRUE) 
{
    if (length(a) < 2 | length(b) < 2) {
        stop(call. = FALSE, "Inputs need to be greater at least 2 elements long")
    }
    if (is.numeric(a) == FALSE | is.numeric(b) == FALSE) {
        stop(call. = FALSE, "Input needs to be numeric.")
    }
    
    mu_a <- mean(a, na.rm = TRUE)
    mu_b <- mean(b, na.rm = TRUE)
    var_a <- var(a, na.rm = TRUE)
    var_b <- var(b, na.rm = TRUE)
    
    # if lengths are equal assume correlation and calculate covariance
    if (length(a) == length(b)) {
        cov_ab <- cov(a, b)
        beta <- (mu_a - mu_b)/sqrt(var_a + var_b - 2 * cov_ab)
    } else{ # unequal lengths => not paired , cannot calc covariance
        beta <- (mu_a - mu_b) / sqrt(var_a + var_b)
        if(verbose == TRUE)
        {
            warning("a and b have different lengths. Calculations assumed no correlation.",
                    call. = FALSE)
        }
    }
    
    
    if (verbose == TRUE) {
        diff <- abs(beta)
        if (diff == 0) 
            message("No effect")
        if (diff <= 0.25 & diff > 0) 
            message("Extremely weak")
        if (diff <= 0.5 & diff > 0.25) 
            message("Very weak")
        if (diff <= 0.75 & diff > 0.5) 
            message("Weak")
        if (diff <= 1 & diff > 0.75) 
            message("Fairly weak")
        if (diff <= 1.28 & diff > 1) 
            message("Fairly moderate")
        if (diff <= 1.654 & diff > 1.28) 
            message("Moderate")
        if (diff <= 2 & diff > 1.654) 
            message("Fairly strong")
        if (diff <= 3 & diff > 2) 
            message("Strong")
        if (diff <= 5 & diff > 3) 
            message("Very strong")
        if (diff > 5) 
            message("Extremely strong (yay)")
    }
    return(beta)
}
