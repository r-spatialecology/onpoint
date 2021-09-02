
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

![R-CMD-check](https://github.com/r-spatialecology/onpoint/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/r-spatialecology/onpoint/branch/main/graph/badge.svg)](https://codecov.io/gh/r-spatialecology/onpoint)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

# onpoint

`onpoint` is a growing collection of helper functions mainly related to
point pattern analysis and [spatstat](http://spatstat.org/).

## Installation

You can install the released version of shar from CRAN with:

``` r
install.packages("onpoint")
```

You can install the development version of `onpoint` from
[Github](https://github.com/r-spatialecology/onpoint) with:

``` r
remotes::install_github("r-spatialecology/onpoint")
```

``` r
library(onpoint)
library(spatstat.core)
library(spatstat.data)

data(spruces)
```

### Summary functions

Currently, `onpoint` provides three second-order summary functions,
namely Besag’s L-function centered to zero and the O-ring statistic and
a fast estimation of the pair-correlation function.

Centering Besag’s L-function to zero has the advantage of an easier
interpretation and plotting (Haase 1995). The function
`center_l_function()` can either deal with a point pattern and
calculated the centered L-function directly, or center the L-function
afterwards it was calculating using `spatstat`s `Lest()`.

``` r
# calculate L-function
l_function <- Lest(spruces, correction = "Ripley")

# center L-function to zero
# center_l_function <- center_l_function(l_function)
l_function_centered <- center_l_function(spruces, correction = "Ripley")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

The O-ring statistic O(r) (Wiegand & Moloney 2004) can be calculated
using `estimate_o_ring()`. Generally speaking, O(r) scales the pair
correlation g(r) function with help of the intensity . One advantage of
the O-ring statistic is that it can be interpreted as a neighborhood
density because it is a probability density function (Wiegand & Moloney
2004).

``` r
o_ring <- estimate_o_ring(spruces)
```

Of course, both summary functions can be used in combination with
`spatstat`’s `envelope()` function.

``` r
oring_envelope <- envelope(spruces, fun = estimate_o_ring, nsim = 199, verbose = FALSE)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

`estimate_pcf_fast()` estimates the pair-correlation function based on
Ripley’s K-function, which is faster than estimation the
pair-correlation function directly.

``` r
estimate_pcf_fast(spruces)
#> Function value object (class 'fv')
#> for the function r -> g(r)
#> .............................................................
#>      Math.label Description                                  
#> r    r          distance argument r                          
#> theo g[pois](r) theoretical Poisson value of g(r)            
#> pcf  g(r)       estimate of g(r) by numerical differentiation
#> .............................................................
#> Default plot formula:  .~r
#> where "." stands for 'pcf', 'theo'
#> Recommended range of argument r: [0, 9.5]
#> Available range of argument r: [0, 9.5]
#> Unit of length: 1 metre
```

### Null models

`onpoint` includes two functions to simulate null model patterns.

`simulate_heterogenous_pattern()` is a convienent wrapper around a few
`spatstat` functions to straighforward simulate a heterogeneous Poisson
process.

``` r
null_model_hetero <- simulate_heterogenous_pattern(spruces, nsim = 199)

hetero <- envelope(spruces, fun = pcf, 
                   funargs = list(correction = "Ripley", divisor = "d"),
                   simulate = null_model_hetero, nsim = 199, 
                   verbose = FALSE)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

To simulate antecedent conditions in which only one pattern influences
the other, but not the other way around (Wiegand & Moloney 2004,
Velazquez et al. 2016), `simulate_antecedent_conditions()` can be used.
This null model randomizes only one type of points (e.g. seedlings),
while keeping the other type of points constant (e.g. mature trees) to
check for associations between the two.

``` r
marks(spruces) <- ifelse(marks(spruces) > 0.3, yes = "adult", no = "seedling")

null_model_antecedent <- simulate_antecedent_conditions(spruces, 
                                                        i = "seedling", j = "adult", nsim = 199)

antecedent <- envelope(spruces, fun = pcf, 
                       funargs = list(correction = "Ripley", divisor = "d"),
                       simulate = null_model_antecedent, nsim = 199, 
                       verbose = FALSE)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

### Various

To plot simulation envelopes using quantum plots (e.g. Esser et
al. 2015), just pass an `envelope` object as input to
`plot_quantums()`.

``` r
plot_quantums(antecedent, ylab = "g(r)")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

#### References

Besag, J. E. 1977. Discussion on Dr. Ripley’s paper. - J. R. Stat. Soc.
Ser. B 39: 193-195.

Esser, D. S. et al. 2015. Spatial scales of interactions among bacteria
and between bacteria and the leaf surface. - FEMS Microbiol. Ecol. 91:
fiu034.

Haase, P. 1995. Spatial pattern analysis in ecology based on Ripley’s
K-function: Introduction and methods of edge correction. - J. Veg. Sci.
6: 575-582.

Velazquez, E. et al. 2016. An evaluation of the state of spatial point
pattern analysis in ecology. - Ecography (Cop.). 39: 1-14.

Wiegand, T. and Moloney, K. A. 2004. Rings, circles, and null models for
point pattern analysis in ecology. - Oikos 104: 209-229.
