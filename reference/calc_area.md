# calc_area

Calculate area of polygon

## Usage

``` r
calc_area(x)
```

## Arguments

- x:

  matrix with x,y coordinates.

## Value

numeric

## Details

Calculate area of polygon in input units. If the polygon is not closed,
the first coordinate is used as last coordinate to close it.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- matrix(data = c(0, 0, 0, 10, 10, 10, 10, 0), ncol = 2, byrow = TRUE)
calc_area(x = dat)
} # }
```
