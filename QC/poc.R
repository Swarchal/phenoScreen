############################################################
# poc()
# Percentage Of Control
#----------------------------------------------------------
# a qualitiative measure of test compound activity
# compound activity as a percentage of postive control
#----------------------------------------------------------
# `compound` should be values of selected compound
# `pos_cntrl` values from positive control
############################################################

poc <- function(compound, pos_cntrl){
    x <- (mean(compound, na.rm = TRUE)/ mean(pos_cntrl, na.rm = TRUE)) * 100
    return(x)
}