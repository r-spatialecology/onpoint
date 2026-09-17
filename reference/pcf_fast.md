# pcf_fast

Fast estimation of the pair correlation function

## Usage

``` r
pcf_fast(pattern, ...)
```

## Arguments

- pattern:

  Point pattern.

- ...:

  Arguments passed down to \`Kest\` or \`pcf.fv\`.

## Value

fv.object

## Details

The functions estimates the pair correlation functions based on an
estimation of Ripley's K-function. This makes it computationally faster
than estimating the pair correlation function directly.

It is a wrapper around `Kest` and `pcf.fv` and returns a 'Function value
object' of the `spatstat` package.

## References

Ripley, B.D., 1977. Modelling spatial patterns. Journal of the Royal
Statistical Society. Series B (Methodological) 39, 172–192.
\<https://doi.org/10.1111/j.2517-6161.1977.tb01615.x\>

Stoyan, D., Stoyan, H., 1994. Fractals, random shapes and point fields.
John Wiley & Sons, Chichester, UK. \<isbn:978-0-471-93757-9\>

## See also

[`Kest`](https://rdrr.io/pkg/spatstat.explore/man/Kest.html)  
[`pcf.fv`](https://rdrr.io/pkg/spatstat.explore/man/pcf.fv.html)

## Examples

``` r
set.seed(42)
pattern <- spatstat.random::runifpoint(n = 100)
pcf_fast <- pcf_fast(pattern)
```
