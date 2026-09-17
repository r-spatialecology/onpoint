# balance_points

Balance number of points

## Usage

``` r
balance_points(pattern, n, verbose = TRUE)
```

## Arguments

- pattern:

  ppp object.

- n:

  Either an integer or a ppp object.

- verbose:

  Print messages.

## Value

ppp

## Details

The function balances out the number of points in the input pattern to
either the provided number of points as integer or the same number of
points if a `ppp` object is provided.

## Examples

``` r
set.seed(42)
input <- spatstat.random::rpoispp(lambda = 100)
input_b <- spatstat.random::rpoispp(lambda = 100)

balance_points(pattern = input, n = 110)
#> > Relative difference between pattern and n: 0.03
#> Planar point pattern: 110 points
#> window: rectangle = [0, 1] x [0, 1] units
balance_points(pattern = input, n = input_b)
#> > Relative difference between pattern and n: 0.27
#> Planar point pattern: 83 points
#> window: rectangle = [0, 1] x [0, 1] units
```
