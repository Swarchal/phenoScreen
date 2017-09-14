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

Collapsing single cell data down to an image mean, then remove redundant feature columns.

```r
data %>%
    group_by(Metadata_image_id) %>%
    collapse(mean) %>%
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
