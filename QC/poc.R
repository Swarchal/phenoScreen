############################################################
# poc()
# Percentage Of Control
#----------------------------------------------------------
# a qualitiative measure of test compound activity
# compound activity as a percentage of postive control
#----------------------------------------------------------
# `compound` should be values of selected compound
# `pos_cntrl` values from positive control
#----------------------------------------------------------
# to use with multiple compounds, use poc() within apply()
# or with aggregate()
############################################################

poc <- function(compound, pos_cntrl){
    
    if(is.numeric(compound) == FALSE){
        stop("Enter numerical values for a single compound. For use with multiple compounds, use poc() within *apply() or aggregate()")
    }
    
    # if values for a single compound are given:
    if(is.numeric(compound) == TRUE){
        b_cmpd <- mean(compound, na.rm = TRUE)
        b_pos_cntrl <- mean(pos_cntrl, na.rm = TRUE)
        x <- (mean(b_cmpd) / mean(b_pos_cntrl)) * 100
        return(x)
    }
}