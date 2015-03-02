###############################################################################
# b_score()
#------------------------------------------------------------------------------
# The residual (r_ijp) of the measurement for row `i` and column `j` on the
# `p`th plate is obtained by fitting a two-way median polish.
#
# The residual is defined as the difference between the observed result (y_ijp)
# and the fitted value (yhat_ijp, defined as the estimated average of the plate 
# (mu_p) + estimated systematic measurement column offset for
# column j` on plate p.
#------------------------------------------------------------------------------
# N.B: well identifiers are required to format the data into correct plate
# layout with the column name as 'well'
# val_col is the column number containing the values of interest
###############################################################################

b_score <- function(data, val_col = 2, plot = FALSE){
	
    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    platemap <- mutate(
        data,
        row = as.numeric(match(toupper(substr(well,1,1)),LETTERS)),
        column = as.numeric(substr(well,2,5))
    )
    
    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$row, platemap$column), ]
    
    mat_plate_map <- matrix(platemap[,val_col],
                            nrow = 8,
                            ncol = 12,
                            byrow = TRUE)
    
	# median polish of the data
	data_pol <- medpolish(mat_plate_map)
    
	return(data_pol$residuals)

	# how  best to normalise? actual - residuals?
	# return diagnostic plot? plot(data_pol)
}
