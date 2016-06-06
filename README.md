[![build_status](https://travis-ci.org/Swarchal/phenoScreen.svg?branch=master)](https://travis-ci.org/Swarchal/phenoScreen/)
[![codecov.io](https://codecov.io/github/Swarchal/phenoScreen/coverage.svg?branch=master)](https://codecov.io/github/Swarchal/phenoScreen?branch=master)

# phenoScreen

An R package for analysing and plotting multivariate screening data in 96 and 384-well plates.

To install with the devtools package:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github('Swarchal/phenoScreen')
```

### Examples

#### Plate maps

Can plot platemaps from well-labels, i.e 'A01' for both 96 and 384 well plates.

```r
# example data
df <- data.frame(vals = rnorm(1:384),
                 well = num_to_well(1:384, plate = 384))

raw_map(data = df$vals,
	    well = df$well,
	    plate = 384)
```

![example plate](/graphics/example_plate.png)

The plots can be treated like any ggplot object and chained with additional functions:

```r
raw_map(data = df$vals,
	    well = df$well,
	    plate = 384) +
    ggtitle("Platemap") +
    theme_dark() +
    scale_fill_viridis()
```

![example_plate2](/graphics/example_plate_2.png)


The platemaps handle missing wells and partial plates:
```r

df_partial <- dplyr::sample_frac(df, 0.7)

raw_map(data = df_partial$vals,
	    well = df_partial$well,
	    plate = 384) +
    ggtitle("Partial Plate")
```

![partial_plate](/graphics/partial_plate.png)

#### Scaling and multiple plates

Values can be scaled via z-score, and also multiple plates can be plotted in a single plot.

```r
vals <- c(rnorm(96), rnorm(96, mean = 10))
wells <- rep(num_to_well(1:96), 2)
plate_id <- rep(c("plate_1", "plate_2"), each = 96)

z_grid(data = vals,
	   well = wells,
	   plate_id = plate_id) +
    ggtitle("Two very different plates")
```

![different_plates](/graphics/different_plates.png)

When the difference in values between two plates is large, the colour scale can become useless. To get around this you can scale each plate separately with the `each` parameter.

```r
z_grid(data = vals,
	   well = wells,
	   plate_id = plate_id,
	   each = TRUE) +
    ggtitle("Plates scaled separately")
```

![different_each](/graphics/different_each.png)

#### Normalising plate effects

Edge effects can be removed with a median polish (or B-score), this can be performed on data while it's still in tabular form without converting to a matrix.
```r
z_map(df_edge$vals,
	  df_edge$well,
	  plate = 384)  +
    ggtitle("Plate with an edge effect") +
    theme_dark() +
    viridis::scale_fill_viridis(option = "A")
```

![edge_plate](/graphics/z_map.png)

```r

b_map(df_edge$vals,
	  df_edge$well,
	  plate = 384) +
    ggtitle("Median polish to remove edge effect") +
    theme_dark() +
    viridis::scale_fill_viridis(option = "A")
```

![edge_fixed](/graphics/b_map.png)

### Grammar

Prefix:
	- `raw`: raw values
	- `z`: z-scored values
	- `b`: b-scored values

Suffix:
	- `map`: single plate map
	- `grid`: multple plate maps

e.g:  
`z_map`: z-scored plate map  
`raw_grid:` raw values, multiple plate maps

------------

Currently a work in progress, version no. `0.3`.
