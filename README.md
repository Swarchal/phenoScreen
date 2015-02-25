# pheno_screen

All functions to be used in phenotypic screens.

- `read_map`: annotates dataframes with well identifiers from a separate plate map as a csv. e.g adding compounds or concentrations to correct wells

- `num_to_well`: converts well numbers into well ID's. e.g 1 -> "A01", 96 -> "H12" *incomplete*

- `well_to_num`: converts well ID's into well numbers. *incomplete*

* QC : Quality control measures

	- `cv_check`: calculates the coefficient of variation if given a column of measurements, and a column with which to group the elements by. Produces a dataframe containing the CV for each element of the group. 

	- `cv_plot`: similar to cv_check, but produces a graph of coefficient of variation (y) against group (x) instead of a data frame.

	- `z_map`: plots normalised values (z-score) as colour heatmap in a plate format.

	- `z_factor`: calculates a z-factor or z` factor to assess how robust the difference is between groups.

	- `poc`: 'Percentage Of Control', measures compound activity as a percentage of a positive control.

	- `npi`: 'Normalised Percentage inhibition', for use in antagonism assays. Calculates the percentage inhibition normalised against the positive and negative controls.

	- `ssmd`: 'Strictly standardised mean difference', calculates the difference between two groups.

	- `mad`: 'Median Absolute Deviation': *incomplete*

	- `b_score` *incomplete*
