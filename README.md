
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LSMS Sampling Trainer Application

<!-- style in red -->
<p float="center">
<img src="warning.svg" width="100%" />
</p
&#10;<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div align="justify">

The LSMS Sampling Trainer Application is a tool to train users in the
basic principles of sampling for household surveys. The application is
based on the [LSMS Sampling
Manual](https://www.worldbank.org/en/programs/lsms/publication/lsms-sampling-manual).
It covers the following topics:

- Introduction to sampling
- Simple random sampling
- Stratified random sampling
- Cluster sampling

It does this by using a synthetic population of Ethiopia and a sample of
this population. The user can select the sample design and the sample
size. The application then shows the sample and the population on a map
and in a table. The user can then compare the sample and the population
and see how the sample design affects the sample. The application also
shows the sample size and the sampling error.

## Installation

- Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1 or
  greater)

- Install R Studio: <https://rstudio.com/products/rstudio/download/>
  (version 1.2.5001-3 or newer)

- Make sure the *devtools* package is installed, if not install it with:

``` r
install.packages("devtools")
```

- After that install the actual package:

``` r
devtools::install_github("michael-cw/lsmssamptrain")
```

## Start the application from RStudio

``` r
library(lsmssamptrain)
lsmssamptrain::runSampleTrainer()
```

## Start the application on a Shiny Server

In case you are considering to run the application on a shiny server,
you just need to create the following app.R script in your shiny server
app directory:

``` r
library(lsmssamptrain)
lsmssamptrain::runSampleTrainerServer()
```

</div>
