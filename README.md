# pheno_screen

All functions to be used in phenotypic screens.

* QC : Quality control measures

	- `cv_check`: calculates the coefficient of variation if given a column of measurements, and a column with which to group the elements by. Produces a dataframe containing the CV for each element of the group. 

	- `z_factor.R`: calculates a z-factor or z` factor to assess how robust the difference is between groups. A z-value of 0.5 equates to a difference of 12 standard deviations between the two groups. 
