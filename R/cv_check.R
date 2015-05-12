cv_check <-
function(data, group){
    
    # to calculate CV
    CV <- function(data){
        (sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE))*100
    }
    
    # calculate CV for given data and groups
    cv_df <- aggregate(data, list(group = group), CV)
    names(cv_df)[2] <- "CV"
    return(cv_df)
}
