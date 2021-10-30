
# rcbayes: Bayesian Rogers-Castro Migration Age Schedules

Date: 2021-01-30

The
[`rcbayes`](https://cran.r-project.org/web/packages/rcbayes/index.html)
package is an R package that contains functions to calculate and
estimate Rogers-Castro migration age schedules in a Bayesian framework.

`rcbayes` version 0.1.0 is now available on
[CRAN](https://cran.r-project.org/web/packages/rcbayes/index.html)
(commit 53da0c8).

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

## Getting Started and Installation

To get started with `rcbayes` we recommend taking a look at the vignette
[Rogers Castro Migration Models with
rcbayes](https://cran.r-project.org/web/packages/rcbayes/vignettes/intro_to_rcbayes.html).

You can load the `rcbayes` package as follows:

``` r
# install.packages("rcbayes")
library(rcbayes)
```

Note that this package requires `rstan`. If you have issues installing
`rstan` additional information is available
[here](https://github.com/stan-dev/rstan/wiki), as well as from a
general Google search.

## Model Convergence

For additional tips in getting models to converge, we recommend taking a
look at the vignette [Achieving Model Convergence With
mig\_estimate\_rc](https://cran.r-project.org/web/packages/rcbayes/vignettes/convergence_issues.html).

## Citation

To cite `rcbayes` in publications, please use:

Alexander M, Yeung J, and Riffe T. (2021) rcbayes: An R package of tools
for Bayesian Rogers-Castro Migration Age Schedules URL:
<https://github.com/jessieyeung/rcbayes/>.

    @Misc{rcbayes,
      Title     = {rcbayes: {A}n {R} package for Bayesian Rogers-Castro Migration Age Schedules},
      Author    = {Alexander, M and Yeung, J and Riffe, T},
      Year      = {2021},
      note      = {URL:~\url{https://github.com/jessieyeung/rcbayes/}}
    }
