# plot.env_summarized

Plotting method for `env_summarized` object

## Usage

``` r
# S3 method for class 'env_summarized'
plot(
  x,
  col = c("#97CBDE", "#E1B0B5"),
  x_lab = NULL,
  y_lab = NULL,
  base_size = 10,
  label = TRUE,
  ...
)
```

## Arguments

- x:

  Random patterns.

- col:

  Colors for areas above and below envelope.

- x_lab, y_lab:

  Labels of x- and y-axis.

- base_size:

  Base size of plot

- label:

  If TRUE the ratios of the area above and below are added to the plot.

- ...:

  To be generic for plotting function.

## Value

ggplot

## Details

Plotting method for summarized envelope created with
[`summarize_envelope`](https://r-spatialecology.github.io/onpoint/reference/summarize_envelope.md).

Returns a `ggplot` object.

## See also

[`summarize_envelope`](https://r-spatialecology.github.io/onpoint/reference/summarize_envelope.md)

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

x <- summarize_envelope(cluster_env)
plot(x)

```
