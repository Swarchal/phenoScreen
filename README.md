# phenoScreen

[![build_status](https://travis-ci.org/Swarchal/phenoScreen.svg?branch=master)](https://travis-ci.org/Swarchal/phenoScreen/)
[![Codecov](https://img.shields.io/codecov/c/github/Swarchal/phenoScreen.svg)](https://codecov.io/github/Swarchal/phenoScreen?branch=master)

An R package for analysing and plotting multivariate screening data in 96 and 384-well plates.

**Pretty broken as dplyr is unstable and keeps breaking things** :thumbsdown:

**Note:** Functions related specifically to multi-well plates have been moved to the [`platetools`](https://www.github.com/swarchal/platetools) package which is now on CRAN :tada:

To install with the devtools package:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github('Swarchal/phenoScreen')
```
