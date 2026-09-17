# center_Lest

Centered L-function

## Usage

``` r
center_Lest(x, ...)
```

## Arguments

- x:

  ppp

- ...:

  Arguments passed to
  [`spatstat.explore::Lest()`](https://rdrr.io/pkg/spatstat.explore/man/Lest.html)

## Value

fv.object

## Details

Centers Besag's L-function to zero by calculating L(r) -r. Centering the
L-function allows an easier interpretation and plotting of the results
(Haase 1995).

Returns an 'Function value object' of the `spatstat` package.

## References

Besag, J.E., 1977. Discussion on Dr. Ripley’s paper. Journal of the
Royal Statistical Society. Series B (Methodological) 39, 193–195.
\<https://doi.org/10.1111/j.2517-6161.1977.tb01616.x\>

Ripley, B.D., 1977. Modelling spatial patterns. Journal of the Royal
Statistical Society. Series B (Methodological) 39, 172–192.
\<https://doi.org/10.1111/j.2517-6161.1977.tb01615.x\>

Haase, P., 1995. Spatial pattern analysis in ecology based on Ripley’s
K-function: Introduction and methods of edge correction. Journal of
Vegetation Science 6, 575–582. \<https://doi.org/10.2307/3236356\>

## See also

[`Lest`](https://rdrr.io/pkg/spatstat.explore/man/Lest.html)

## Examples

``` r
input_pattern <- spatstat.random::runifpoint(n = 100)
center_Lest(input_pattern, correction = "Ripley")
#> Function value object (class ‘fv’)
#> for the function r -> L(r)-r
#> ............................................................
#>      Math.label       Description                           
#> r    r                distance argument r                   
#> theo L[pois](r)-r     theoretical Poisson L(r)-r            
#> iso  hat(L)[iso](r)-r isotropic-corrected estimate of L(r)-r
#> ............................................................
#> Default plot formula:  .~.x
#> where “.” stands for ‘iso’, ‘theo’
#> Recommended range of argument r: [0, 0.25]
#> Available range of argument r: [0, 0.25]

lest <- spatstat.explore::Lest(input_pattern)
center_Lest(lest)
#> Function value object (class ‘fv’)
#> for the function r -> L(r)-r
#> ..................................................................
#>        Math.label         Description                             
#> r      r                  distance argument r                     
#> theo   L[pois](r)-r       theoretical Poisson L(r)-r              
#> border hat(L)[bord](r)-r  border-corrected estimate of L(r)-r     
#> trans  hat(L)[trans](r)-r translation-corrected estimate of L(r)-r
#> iso    hat(L)[iso](r)-r   isotropic-corrected estimate of L(r)-r  
#> ..................................................................
#> Default plot formula:  .~.x
#> where “.” stands for ‘iso’, ‘trans’, ‘border’, ‘theo’
#> Recommended range of argument r: [0, 0.25]
#> Available range of argument r: [0, 0.25]
```
