###########################################################
# b_score()
#---------------------------------------------------------
# The residual (r_ijp) of the measurement for row `i` and
# column `j` on the `p`th plate is obtained by fitting a
# two-way median polish.
#
# The residual is defined as the difference between the
# observed result (y_ijp) and the fitted value (yhat_ijp,
# defined as the estimated average of the plate (mu_p) +
# estimated systematic measurement column offset for column
# `j` on plate p.
#
# For each plate `p`, the adjusted median absolute deviation
# (MAD_p) is obtained from the `r_ijp`s (MAD_p).
###########################################################

b_score <- function(){
	
}