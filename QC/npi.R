###########################################################
# npi()
#----------------------------------------------------------
# Normalised percentage inhibition
# for use in antagonist assays
#----------------------------------------------------------
# `pos_ctrl` positive control values
# `neg_ctrl` negative control values
# `compound` values for compound i
# #########################################################

npi <- function(compound, pos_ctrl, neg_ctrl) {

	b_p_ctrl <- as.vector(mean(pos_ctrl, na.rm = TRUE))
	b_n_ctrl <- as.vector(mean(neg_ctrl, na.rm = TRUE))
	b_cmpd   <- as.vector(mean(compound, na.rm = TRUE))

	NPI <- ((b_p_ctrl - b_cmpd) / (b_p_ctrl - b_n_ctrl))
	return(NPI)
}
