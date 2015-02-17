#################################################
# cv_plot()
#----------------------------------------
# makes a plot of coefficient variation plot
# from the values obtained from cv_check()
#################################################

cv_plot <- function(data, group){
    
    # to calculate CV
    CV <- function(data){
        (sd(data, na.rm = TRUE)/mean(data, na.RM = TRUE))*100
    }
    
    # calculate CV for given data and groups
    cv_df <- aggregate(data, list(group = group), CV)
    names(cv_df)[2] <- "CV"
    
    plt <- plot(
        cv_df[,1],
        cv_df[,2],
        type = "b",
        ylim = c(0, max(cv_df[,2])),
        ylab = "CV",
        xlab = "Group",
        main = "CV Plot")
    
    return(plt)
}