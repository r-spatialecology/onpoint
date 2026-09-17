# Oest

O-ring function

## Usage

``` r
Oest(x, ...)
```

## Arguments

- x:

  ppp

- ...:

  Arguments passed to
  [`spatstat.explore::pcf.ppp()`](https://rdrr.io/pkg/spatstat.explore/man/pcf.ppp.html)

## Value

fv.object

## Details

Estimates the O-ring function proposed by Wiegand and Moloney (2004).
The O-ring statistic is defined as:

\$\$O(r) = \lambda \* g(r)\$\$

Generally speaking, O(r) scales the pair correlation g(r) function with
help of the intensity \\\lambda\\. One advantage of the O-ring statistic
is that it can be interpreted as a neighborhood density because it is a
probability density function (Wiegand & Moloney 2004, 2014).

Returns an 'Function value object' of the `spatstat` package.

## References

Wiegand, T., Moloney, K.A., 2004. Rings, circles, and null models for
point pattern analysis in ecology. Oikos 104, 209–229.
\<https://doi.org/10.1111/j.0030-1299.2004.12497.x\>

Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern
analysis in ecology. Chapman and Hall/CRC Press, Boca Raton, USA.
\<isbn:978-1-4200-8254-8\>

## See also

[`density.ppp`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html)  
[`pcf`](https://rdrr.io/pkg/spatstat.explore/man/pcf.html)

## Examples

``` r
input_pattern <- spatstat.random::runifpoint(n = 100)
Oest(input_pattern)
#> Warning: default settings have changed for pcf.ppp in spatstat.explore >= 3.8-1
#> Function value object (class ‘fv’)
#> for the function r -> g(r)*lambda
#> ............................................................................
#>       Math.label               Description                                  
#> r     r                        distance argument r                          
#> theo  g[Pois](r)*lambda        theoretical Poisson g(r)*lambda              
#> trans hat(g)[Trans](r)*lambda  translation-corrected estimate of g(r)*lambda
#> iso   hat(g)[Ripley](r)*lambda isotropic-corrected estimate of g(r)*lambda  
#> ............................................................................
#> Default plot formula:  .~.x
#> where “.” stands for ‘iso’, ‘trans’, ‘theo’
#> Recommended range of argument r: [0, 0.25]
#> Available range of argument r: [0, 0.25]
```
