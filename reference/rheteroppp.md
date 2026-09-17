# rheteroppp

Simulate heterogeneous pattern

## Usage

``` r
rheteroppp(x, nsim, fix_n = FALSE, ...)
```

## Arguments

- x:

  ppp

- nsim:

  Number of patterns to simulate.

- fix_n:

  Logical if true the null model patterns have exactly the same number
  of points ais input.

- ...:

  Arguments passed to
  [`spatstat.explore::density.ppp()`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html).

## Value

list

## Details

Simulate heterogeneous point patterns as null model data for
[`spatstat.explore::envelope()`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html).
A heterogeneous Poisson process is used, meaning that there are no
interaction between points, however, the simulated coordinates depend on
the intensity \\\lambda\\ of the input pattern.

Returns a `list` with `ppp` objects.

## References

Baddeley, A., Rubak, E., Turner, R., 2015. Spatial point patterns:
Methodology and applications with R. Chapman and Hall/CRC Press, London,
UK. \<isbn:978-1-4822-1020-0\>

Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern
analysis in ecology. Chapman and Hall/CRC Press, Boca Raton, USA.
\<isbn:978-1-4200-8254-8\>

## See also

[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)  
[`density.ppp`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html)

## Examples

``` r
set.seed(42)
input_pattern <- spatstat.random::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)
null_model <- rheteroppp(input_pattern, nsim = 19)
spatstat.explore::envelope(Y = input_pattern, fun = spatstat.explore::pcf, nsim = 19,
simulate = null_model)
#> Extracting 19 point patterns from list  ...
#> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
#> 19.
#> 
#> Done.
#> Pointwise critical envelopes for g(r)
#> and observed value for ‘input_pattern’
#> Edge correction: “iso”
#> Obtained from 19 point patterns from list
#> Alternative: two.sided
#> Significance level of pointwise Monte Carlo test: 2/20 = 0.1
#> ......................................................................
#>       Math.label     Description                                      
#> r     r              distance argument r                              
#> obs   hat(g)[obs](r) observed value of g(r) for data pattern          
#> mmean bar(g)(r)      sample mean of g(r) from simulations             
#> lo    hat(g)[lo](r)  lower pointwise envelope of g(r) from simulations
#> hi    hat(g)[hi](r)  upper pointwise envelope of g(r) from simulations
#> ......................................................................
#> Default plot formula:  .~r
#> where “.” stands for ‘obs’, ‘mmean’, ‘hi’, ‘lo’
#> Columns ‘lo’ and ‘hi’ will be plotted as shading (by default)
#> Recommended range of argument r: [0, 0.25]
#> Available range of argument r: [0, 0.25]
```
