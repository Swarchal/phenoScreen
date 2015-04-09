# pheno_screen

All functions to be used in phenotypic screens.

- `read_map`: annotates dataframes with well identifiers from a separate plate map as a csv. e.g adding compounds or concentrations to correct wells

- `num_to_well`: converts well numbers into well ID's. e.g 1 -> "A01", 96 -> "H12".

- `well_to_num`: converts well ID's into well numbers.

- `load_csv`: loads all csv files within a folder into the working environment.

- `rbind_all`: reads all csv files in a directory and binds them together with identifying column.

- `rm_col_na`: removes columns from a dataframe consisting entirely of NA's.

* **QC : Quality control measures**

	- `cv_check`: calculates the coefficient of variation if given a column of measurements, and a column with which to group the elements by. Produces a dataframe containing the CV for each element of the group. 

	- `cv_plot`: similar to cv_check, but produces a graph of coefficient of variation (y) against group (x) instead of a data frame.

	- `z_map`: plots normalised values (z-score) as colour heatmap in a plate format.

	- `z_factor`: calculates a z-factor or z` factor to assess how robust the difference is between groups.

	- `z_factors_scan`: calculates z-factor between two treatments groups across multiple variables, returns those above a specified cut-off.

	- `poc`: 'Percentage Of Control', measures compound activity as a percentage of a positive control.

	- `npi`: 'Normalised Percentage inhibition', for use in antagonism assays. Calculates the percentage inhibition normalised against the positive and negative controls.

	- `ssmd`: 'Strictly standardised mean difference', calculates the difference between two groups.

	- `b_score`: A correction of plate and spatial effects by a two-way median polish to account for row and column variability.

	- `b_score_scan`: correction of plate and spatial effects by a two-way median polish, applied to each feature in a dataset.

	- `hit_map`: plots a platemap, colours indicate 'hits' if they exceed a specified threshold of standard deviation from the plate mean.

	- `pc_map`: plots plate heatmap of first principcal component.

	- `ld_map`: plots a plate heatmap of the first dimension of a linear discriminant analysis separated by a given groups.
	
	- `raw_map`: plots plate heatmap of raw untransformed values.

	- `dist_map`: plots platemap of probability distributions of a selected feature. Useful for looking at heterogeneity.

	- `plot_confidence`: calculates bivariate confidence interval via 2D kde and plots as contour on scatter plot.

