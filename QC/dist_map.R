###############################################################################
# dist_map
#------------------------------------------------------------------------------
# - plots probability distribution of a given feature in a platemap format
# - requires columns indicating well row and column position
#       - obtained externally through num_to_well() function
# argument:
#           - 'data': dataframe containing all columns used
#           - 'feature': numerical value indicating column no. of feature
#           - 'title': self-explanatory
#           - 'row': column containing row values
#           - 'column': column containing column values
# row and column arguments default correctly if initially passed through
# well_to_num function
#------------------------------------------------------------------------------
# e.g: to get the distribution of the features contained in the 5th column of
# the dataframe 'df'
# dist_map(data = df, feature = 5, title = "Example Plot")
###############################################################################

dist_map <- function(data, feature, row, column, title = ""){
    
    require(ggplot2)
    
    localenv <- environment() # 'cause ggplot2 is a mess
    
    plt <- ggplot(data = data,
                  aes(x = data[,feature]),
                  environment = localenv) +  # so the mess recgonises arguments
        geom_density(alpha = 0.6,
                     fill = "gray80") + 
        facet_grid(row ~ column) + 
        xlab(colnames(data)[feature]) + 
        theme_bw() + 
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) # rot. x-axis lab
    
    return(plt)
}
