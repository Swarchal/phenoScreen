# phenoScreen

[![build_status](https://travis-ci.org/Swarchal/phenoScreen.svg?branch=master)](https://travis-ci.org/Swarchal/phenoScreen/)
[![Codecov](https://img.shields.io/codecov/c/github/Swarchal/phenoScreen.svg)](https://codecov.io/github/Swarchal/phenoScreen?branch=master)

Functions for analysing phenotypic screening data, designed to fit in dplyr workflows.

### Examples:

Normalising feature data against negative control values within plates, then scaling features via a z-score, and replace features with 4 principal components.
```r
data %>%
    group_by(Metadata_plate_name) %>%
    normalise(Metadata_compound, neg_control = "DMSO") %>%
    ungroup() %>%
    scale_features() %>%
    pca(n_components = 5)
```

-----------

Collapsing single cell data down to an image mean
```r
data %>%
    group_by(Metadata_image_id) %>%
    collapse(mean)

```
------------

To install from github:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github('Swarchal/phenoScreen')
```
