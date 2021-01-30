
# rcbayes: Bayesian Rogers-Castro Migration Age Schedules

Date: 2021-01-30

The `rcbayes` package is an R package that contains functions to
calculate and estiamte Rogers-Castro migration age schedules in a
Bayesian framework. It is in active development.

`rcbayes` is a spin-off package that is called on by
[`DemoTools`](https://github.com/timriffe/DemoTools/), but can also be
used as a stand-alone package. `DemoTools` is an R package that contains
simple functions often used in demographic analysis.

This project, including both the `rcbayes` and `DemoTools` packages, is
commissioned by the [UN Population
Division](http://www.un.org/en/development/desa/population/) and
financed by the [Bill and Melinda Gates
Foundation](https://www.gatesfoundation.org/) as part of the [Making
Family Planning
Count](http://www.un.org/en/development/desa/population/projects/making-family-planning-count/index.shtml)
project. `rcbayes` was created in collaboration with [Monica
Alexander](https://www.monicaalexander.com/) and [Tim
Riffe](https://github.com/timriffe/). This work is licensed under the
MIT License.

## Installation

You can install the released version of rcbayes from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rcbayes")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jessieyeung/rcbayes")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rcbayes)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
