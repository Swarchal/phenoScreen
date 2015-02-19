###############################################################
# cv_plot()
#----------------------------------------
# makes a plot of coefficient variation plot
# from the values obtained from cv_check()
#
# fancy argument produces plot in ggplot,
# trend argument adds connecting lines between points
# rotate argument rotates the y-axis labels 90 degrees
#################################################################

cv_plot <- function(data, group,
                    fancy = FALSE,
                    trend = FALSE,
                    rotate = FALSE){
    
    # to calculate CV
    CV <- function(data){
        (sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE)) * 100
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
    
    if(fancy == TRUE){
        require(ggplot2)
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
                          geom_point() + 
                          ylim(c(0, max(cv_df$CV))) + 
                          ylab("CV") + 
                          xlab("Group") + 
                          ggtitle("CV Plot")
    }
    
    if(fancy == TRUE & rotate == TRUE){
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
            geom_point() + 
            ylim(c(0, max(cv_df$CV))) + 
            ylab("CV") + 
            xlab("Group") + 
            ggtitle("CV Plot")
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    if(fancy == TRUE & trend == TRUE) {
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
            geom_point() + 
            geom_line(alpha = 0.6,
                      aes(group = "none")) + 
            ylim(c(0, max(cv_df$CV))) + 
            ylab("CV") + 
            xlab("Group") + 
            ggtitle("CV Plot")
    }
    
    if(fancy == TRUE & trend == TRUE & rotate == TRUE){
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
            geom_point() + 
            geom_line(alpha = 0.6,
                      aes(group = "none")) + 
            ylim(c(0, max(cv_df$CV))) + 
            ylab("CV") + 
            xlab("Group") + 
            ggtitle("CV Plot") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    return(plt)
}
