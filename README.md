# pheno_screen

All functions to be used in phenotypic screens.

* QC : Quality control measures

	- `cv_check`: calculates the coefficient of variation if given a column of measurements, and a column with which to group the elements by. Produces a dataframe containing the CV for each element of the group. 

	- `cv_plot`: similar to cv_check, but produces a graph of coefficient of variation (y) against group (x) instead of a data frame. Less flexible than manually calling plot on `cv_check` results. Optional arguments to produce plot in ggplot, with additional options to add connecting line between points and rotate x-axis labels by 90 degrees.

	- `z_factor.R`: calculates a z-factor or z` factor to assess how robust the difference is between groups. A z-value of 0.5 equates to a difference of 12 standard deviations between the two groups. 
