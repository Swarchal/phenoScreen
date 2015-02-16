# pheno_screen

All functions to be used in phenotypic screens.

* QC : Quality control measures
	- `cv_check`: calculates the coefficient of variation if given a column of measurements, and a column with which to group the elements by. Produces a dataframe containing the CV for each element of the group. 
