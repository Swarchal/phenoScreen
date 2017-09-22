# phenoScreen

[![build_status](https://travis-ci.org/Swarchal/phenoScreen.svg?branch=master)](https://travis-ci.org/Swarchal/phenoScreen/)
[![Codecov](https://img.shields.io/codecov/c/github/Swarchal/phenoScreen.svg)](https://codecov.io/github/Swarchal/phenoScreen?branch=master)

Functions for analysing phenotypic screening data, designed to fit in dplyr workflows.

Functions related to plotting and manipulating 96/384 well plates have been moved to the [platetools](https://www.github.com/swarchal/platetools) package.

## Examples

Normalising feature data against negative control values within plates, then scaling features via a z-score, and replace features with 5 principal components.

```r
data %>%
    group_by(Metadata_plate_name) %>%
    normalise(Metadata_compound, neg_control = "DMSO") %>%
    ungroup() %>%
    scale_features() %>%
    pca(n_components = 5)
```

-----------

Average single cell data down to an image mean, then remove redundant feature columns.

```r
data %>%
    group_by(Metadata_image_id) %>%
    squash(mean) %>%
    ungroup() %>%
    remove_correlated(threshold = 0.99)

```

------------

Workflows fit nicely for piping data into ggplot2 for plotting.

```r
data %>%
    group_by(Metadata_plate_name) %>%
    normalise(Metadata_compound, neg_control = "DMSO") %>%
    ungroup() %>%
    scale_features() %>%
    pca(n_components = 5) %>%
    ggplot() +
        geom_point(aes(PC1, PC2)) +
        theme_minimal()
```

## Installation

To install from github:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github('Swarchal/phenoScreen')
```

## Caveats

PhenoScreen assumes that metadata and featuredata columns are labelled differently, and that all metadata columns share a similar prefix.

By default this is `Metadata`. Though this can be changed globally by changing the settings in `options` at top of your script after loading `phenoScreen`.  
e.g

```r
library(phenoScreen)
library(dplyr)

options("metadata_prefix") = "New_Metadata_prefix"

# do stuff here
# ...
```

Or, within each function with the `metadata_prefix` argument.  
e.g

```r
data %>%
    group_by(Metadata_image_id, metadata_prefix = "New_Metadata_prefix") %>%
    squash(mean, metadata_prefix = "New_Metadata_prefix") %>%
    ungroup() %>%
    remove_correlated(threshold = 0.99, metadata_prefix = "New_Metadata_prefix")
```


It is also assumed that all columns that do not have the metadata prefix are featuredata, and that all featuredata is numeric.