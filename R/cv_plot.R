cv_plot <-
function(data, group,
                    fancy = FALSE,
                    trend = FALSE,
                    rotate = FALSE,
                    title = "CV Plot"){
    
    # to calculate CV
    CV <- function(data){
        (sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE)) * 100
    }
    
    # calculate CV for given data and groups
    cv_df <- aggregate(data, list(group = group), CV)
    names(cv_df)[2] <- "CV"
    
    #labels and headings
    x_lab <- "Group"
    y_lab <- "CV"
    title <- title
    
    plt <- plot(
        cv_df[,1],
        cv_df[,2],
        type = "b",
        ylim = c(0, max(cv_df[,2])),
        ylab = y_lab,
        xlab = x_lab,
        main = title)
    
    if(fancy == FALSE & rotate == TRUE){
        plt <- plot(
            cv_df[,1],
            cv_df[,2],
            type = "b",
            ylim = c(0, max(cv_df[,2])),
            xlab = x_lab,
            ylab = y_lab,
            main = title,
            las = 2)
    }
    
    if(fancy == TRUE){
        require(ggplot2)
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
                          geom_point() + 
                          ylim(c(0, max(cv_df$CV))) + 
                          ylab(y_lab) + 
                          xlab(x_lab) + 
                          ggtitle(title)
    }
    
    if(fancy == TRUE & rotate == TRUE){
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
            geom_point() + 
            ylim(c(0, max(cv_df$CV))) + 
            ylab(y_lab) + 
            xlab(x_lab) + 
            ggtitle(title) + 
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
            ylab(y_lab) + 
            xlab(x_lab) + 
            ggtitle(title)
    }
    
    if(fancy == TRUE & trend == TRUE & rotate == TRUE){
        plt <- ggplot(data = cv_df,
                      aes(x = group,
                          y = CV)) +
            geom_point() + 
            geom_line(alpha = 0.6,
                      aes(group = "none")) + 
            ylim(c(0, max(cv_df$CV))) + 
            ylab(y_lab) + 
            xlab(x_lab) + 
            ggtitle(title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    return(plt)
}
