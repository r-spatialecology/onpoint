# simulate_antecedent

Simulate heterogenous pattern

## Usage

``` r
simulate_antecedent(x, i, j, nsim, heterogenous = FALSE, ...)
```

## Arguments

- x:

  ppp

- i:

  Mark of points that are not not changed.

- j:

  Mark of points that are randomized.

- nsim:

  Number of patterns to simulate.

- heterogenous:

  If TRUE, points with the mark j are randomized using a heterogeneous
  Poisson process.

- ...:

  Arguments passed to
  [`spatstat.explore::density.ppp()`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html).

## Value

list

## Details

Simulate point patterns as null model data for
[`spatstat.explore::envelope()`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)
using antecedent conditions as null model. `x` must be a marked point
pattern with two types of marks. Antecedent conditions are suitable as a
null model if points of type i may influence points of type j, but not
the other way around (Velazquez et al 2016). One example are the
positions of seedlings that may be influenced by the position of mature
trees.

Returns a `list` with `ppp` objects.

## References

Velázquez, E., Martínez, I., Getzin, S., Moloney, K.A., Wiegand, T.,
2016. An evaluation of the state of spatial point pattern analysis in
ecology. Ecography 39, 1–14. \<https://doi.org/10.1111/ecog.01579\>

Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern
analysis in ecology. Chapman and Hall/CRC Press, Boca Raton, USA.
\<isbn:978-1-4200-8254-8\>

## See also

[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)

## Examples

``` r
set.seed(42)
pattern_a <- spatstat.random::runifpoint(n = 20)
spatstat.geom::marks(pattern_a) <- "a"
pattern_b <- spatstat.random::runifpoint(n = 100)
spatstat.geom::marks(pattern_b) <- "b"
pattern <- spatstat.geom::superimpose(pattern_a, pattern_b)

null_model <- simulate_antecedent(x = pattern, i = "a", j = "b", nsim = 19)
spatstat.explore::envelope(Y = pattern, fun = spatstat.explore::pcf,
nsim = 19, simulate = null_model)
#> Extracting 19 point patterns from list  ...
#> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
#> 19.
#> 
#> Done.
#> Pointwise critical envelopes for g(r)
#> and observed value for ‘pattern’
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
