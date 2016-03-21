[![build_status](https://travis-ci.org/Swarchal/phenoScreen.svg?branch=master)](https://travis-ci.org/Swarchal/phenoScreen/)
[![codecov.io](https://codecov.io/github/Swarchal/phenoScreen/coverage.svg?branch=master)](https://codecov.io/github/Swarchal/phenoScreen?branch=master)

# phenoScreen

![phenoScreen](/graphics/phenoScreen_banner.png)

An R package for analysing and plotting multivariate screening data in 96 and 384-well plates.

[Examples](http://rstudio-pubs-static.s3.amazonaws.com/90077_45edf515f1b14fab9c2542b6807c6848.html)

To install with the devtools package:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github('Swarchal/phenoScreen')
library(phenoScreen)
```

### Demo

```r
# example data
df <- data.frame(vals = rnorm(1:384),
                 well = num_to_well(1:384, plate = 384))

raw_map(data = df$vals,
        well = df$well,
        title = "Title of plot")
```

![example plate](/graphics/example_plate.png)


------------

Currently a work in progress, version no. `0.3`.
