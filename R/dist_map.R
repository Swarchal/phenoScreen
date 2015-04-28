dist_map <-
function(data, feature, row, column, title = ""){
    
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
