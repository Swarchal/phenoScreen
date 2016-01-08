#' Calculates the coefficient of variation across groups
#'
#' Calculates the coefficient of varation across groups
#'
#' @param data Vector of numerical values
#' @param group Vector of factors with which to group the numerical values
#'
#' @return dataframe with row per group, and a column of CV values
#'
#' @export
#'
#' @examples
#' a <- rnorm(100, 10, 1)
#' b <- rnorm(100, 10, 10)
#' vals <- c(a, b)
#' grouping <- c(rep('a', 100), rep('b', 100))
#' df <- data.frame(vals, grouping)
#' cv_check(data = df$vals,
#'           group = df$grouping)

cv_check <- function(data, group){
    
    # to calculate CV
    CV <- function(data){
        (sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE))*100
    }
    
    # calculate CV for given data and groups
    cv_df <- aggregate(data, list(group = group), CV)
    names(cv_df)[2] <- "CV"
    return(cv_df)
}
