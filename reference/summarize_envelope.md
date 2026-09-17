# summarize_envelope

Summarize simulation envelope

## Usage

``` r
summarize_envelope(x, plot_result = FALSE)
```

## Arguments

- x:

  fv

- plot_result:

  A plot is drawn.

## Value

env_summarized

## Details

The area above and below the null model envelope is divided by the total
area under the curve. If `seperated = TRUE`, the first returning value
is the relative area above, the second value the relative value below
the envelope. If `seperated = FALSE` the value is the absolute sum of
both ratio. If the value is positive, the area above the envelope is
larger than the value below the envelope. If the value is negative, the
area under the envelope is larger than the value above the envelope.

The returned `env_summarized` object includes information about the area
under the curve where the summary function observed pattern is above or
below the null model envelopes.

## See also

[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)

## Examples

``` r
set.seed(42)
input_pattern <- spatstat.random::rThomas(kappa = 15, scale = 0.05, mu = 5)

cluster_env <- spatstat.explore::envelope(input_pattern, fun = "pcf", nsim = 39,
funargs = list(divisor = "d", correction = "Ripley", stoyan = 0.25))
#> Generating 39 simulated realisations of CSR  ...
#> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
#> 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 
#> 39.
#> 
#> Done.

summarize_envelope(cluster_env)
#> Total ratio: 25.28% 
#> ------------ 
#> Above ratio: 25.28% 
#> Below ratio: 0% 
```
